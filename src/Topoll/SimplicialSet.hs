{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Topoll.SimplicialSet where

import Control.Monad
import Data.Bifunctor qualified
import Data.Function (on)
import Data.List (groupBy, sort, sortOn, transpose)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Set qualified as S
import GHC.TypeLits
import Internal.Matrix (Matrix(..))
import Topoll.ChainComplex.Field
import Topoll.ChainComplex.Type

type Simplex  = S.Set Int

{-| Simplicial set is a set that contains all subsets of its elements.
  E.g. [[],1,2,[1,2]] represents a segment with two vertexes (1 and 2),
  and
  [[],
  [1],[2],[3],
  [1,2],[1,3],[2,3]]
  is a border of triangle (the triangle itself is a full simplex that also contain [1,2,3])
 -}
newtype SimplicialSet = UnsafeSimplicialSet (S.Set Simplex) deriving Eq
instance Show SimplicialSet where  show (UnsafeSimplicialSet a) = show $ map S.toAscList (S.toAscList a)

simplicialSet :: S.Set Simplex -> SimplicialSet
simplicialSet = UnsafeSimplicialSet . S.unions . S.map S.powerSet

mkSimplicialSet :: [[Int]] -> SimplicialSet
mkSimplicialSet = simplicialSet . S.fromList . map S.fromList

vertices :: SimplicialSet -> S.Set Int
vertices (UnsafeSimplicialSet s) = S.unions s

neighbours :: SimplicialSet -> Int -> S.Set Int
neighbours (UnsafeSimplicialSet s) n = S.fromList $ do
  simpl <- S.toList s
  if S.size simpl == 2 && S.member n simpl
  then S.toList $ S.delete n simpl
  else []

simplicialSetFullCheck :: [[Int]] -> Maybe SimplicialSet
simplicialSetFullCheck lists = do
  let sorted = sortOn (\x -> (length x, x)) $ map sort lists
  if containsDuplicates sorted
    then Nothing
    else UnsafeSimplicialSet <$> foldM f S.empty sorted
  where
    containsDuplicates :: Eq a => [a] -> Bool -- for sorted lists!!
    containsDuplicates xs = or $ zipWith (==) xs (tail xs)

    f :: S.Set Simplex -> [Int] -> Maybe (S.Set Simplex)
    f s sortedList =
      if containsDuplicates sortedList
        then Nothing
        else let
          simplex = S.fromList sortedList

          in if all (`S.member` s) (M.keys $ simplexBorder simplex) then
            Just $ S.insert simplex s
            else Nothing

{- Set that contains simplexes of one size, like [[1,2], [3,4].
   Of course, constructor `F` is unsafe. -}
newtype SimplexsOfFixedSize = F (S.Set Simplex) deriving (Show, Eq)

splitBySizes :: SimplicialSet -> [SimplexsOfFixedSize]
splitBySizes (UnsafeSimplicialSet s) = map (F . S.fromList) $ groupOn S.size $ sortOn S.size $ S.toAscList s
  where groupOn f = groupBy ((==) `on` f)

simplicialComplex :: forall a. (Num a, Eq a) => SimplicialSet -> SomeChainComplex a
simplicialComplex sc = addZeroToRight $  buildChainComplex $ reverse (tail $ splitBySizes sc) -- tail removes []
  where
    buildChainComplex :: [SimplexsOfFixedSize] -> SomeChainComplex a
    buildChainComplex [] = SomeChainComplex ZeroComplex
    buildChainComplex [F s] = case someNatVal (fromIntegral $ length s) of
      Nothing -> error "No someNatVal?"
      Just (SomeNat (Proxy :: Proxy n)) -> SomeChainComplex . fromJust $ startComplex @n @a
    buildChainComplex (F highestGrade : F prevGrade : rest) = case buildChainComplex (F prevGrade : rest) of
      SomeChainComplex prevCC@(UnsafeAddToRight _ (_ :: Matrix x y a)) -> case someNatVal (fromIntegral $ length highestGrade) of
        Nothing -> error "No someNatVal?"
        Just (SomeNat (Proxy :: Proxy n)) ->
          let
              rows :: [[a]]
              rows = map (\s -> map (\q -> case simplexBorder s M.!? q of
                Nothing -> 0
                Just Plus -> 1
                Just Minus -> -1) $ S.toAscList prevGrade) $ S.toAscList highestGrade

              matrix :: Matrix y n a
              matrix = Matrix (fromInteger $ natVal (Proxy @y), fromInteger $ natVal (Proxy @n))
                (transpose rows)
           in SomeChainComplex $ UnsafeAddToRight prevCC matrix
      SomeChainComplex ZeroComplex -> error "Unexpected ZeroComplex"

    addZeroToRight :: SomeChainComplex a -> SomeChainComplex a
    addZeroToRight (SomeChainComplex cc@(UnsafeAddToRight _ _)) = SomeChainComplex $ UnsafeAddToRight cc (zeroMatrix @_ @0)
    addZeroToRight (SomeChainComplex ZeroComplex) = SomeChainComplex . fromJust $ startComplex @0 @a

simplicialHomologyOverQ :: SimplicialSet -> [BettiNumber]
simplicialHomologyOverQ sc = case simplicialComplex sc of
  SomeChainComplex cc -> bettiNumbers cc
-- >>> let simplicialCircle = mkSimplicialSet [[1,2], [2,3], [1,3]] in simplicialHomologyOverQ simplicialCircle
-- [1,1]


data Sign = Plus | Minus deriving (Show, Eq)
reverseSign :: Sign -> Sign
reverseSign Plus = Minus
reverseSign Minus = Plus

simplexBorder :: Simplex -> M.Map Simplex Sign
simplexBorder = M.fromList . map (Data.Bifunctor.first S.fromAscList) . helper Plus . S.toAscList where
  helper :: Sign -> [Int] -> [([Int], Sign)]
  helper _ [] = []
  helper sign (x:xs) = (xs, sign) : map (Data.Bifunctor.first (x:)) (helper (reverseSign sign) xs)

-- >>> simplexBorder [1,2,3,4,5]
-- fromList [(fromList [1,2,3,4],Plus),(fromList [1,2,3,5],Minus),(fromList [1,2,4,5],Plus),(fromList [1,3,4,5],Minus),(fromList [2,3,4,5],Plus)]

flagN :: Int -> SimplicialSet -> SimplicialSet -- adds simplices up to dimension n with all edges present in original simplicial set
flagN n = foldr ((\f g -> g . f) . flag') id ([1..n] :: [Int])
 where
  flag' :: Int -> SimplicialSet -> SimplicialSet
  flag' n' sc@(UnsafeSimplicialSet s') = UnsafeSimplicialSet . S.union s' . S.fromList $ do
    nSimpl <- S.toList $ S.filter ((== n') . S.size) s'
    let allNeighbours = S.map (neighbours sc) nSimpl
    commonNeighbour <- S.toList $ foldl S.intersection
      (if null allNeighbours then mempty else S.elemAt 0 allNeighbours) allNeighbours
    return $ S.insert commonNeighbour nSimpl

flag :: SimplicialSet -> SimplicialSet -- adds all simplices all of which edges are present. Needs optimization
flag s = flagN (S.size $ vertices s) s

testSC :: SimplicialSet
testSC = mkSimplicialSet [
  [1, 2],
  [2, 3],
  [1, 3],
  [1, 4]
 ]

-- saveSimpl :: SimplicialSet -> IO ()
-- saveSimpl (UnsafeSimplicialSet s) = writeFile "simp.simp" . intercalate "\n" . map (unwords . map show . S.toList) . S.toList $ s

-- >>> mkSimplicialSet [[1, 2],[2, 3],[1, 3],[1, 4] ]
-- [[],[1],[1,2],[1,3],[1,4],[2],[2,3],[3],[4]]

-- >>> flag $ testSC
-- [[],[1],[1,2],[1,2,3],[1,3],[1,4],[2],[2,3],[3],[4]]
