{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Topoll.SimplicialSet where

import Control.Monad
import Data.List (sort, sortOn, groupBy, transpose)
import qualified Data.Set as S
import Topoll.ChainComplex.Field
import qualified Data.Bifunctor
import Data.Function (on)
import Topoll.ChainComplex.Type
import Data.Proxy
import GHC.TypeLits
import Data.Maybe (fromJust)
import QLinear (Matrix)
import Internal.Matrix (Matrix(..))
import qualified Data.Map as M

type Simplex  = S.Set Int

newtype SimplicialSet = UnsafeSimplicialSet (S.Set Simplex)
instance Show SimplicialSet where  show (UnsafeSimplicialSet a) = show $ map S.toAscList (S.toAscList a)

simplicialSet :: S.Set Simplex -> SimplicialSet
simplicialSet = UnsafeSimplicialSet . S.unions . S.map S.powerSet

mkSimplicialSet :: [[Int]] -> SimplicialSet
mkSimplicialSet = simplicialSet . S.fromList . map S.fromList

noDuplicates :: Ord a => [a] -> Bool
noDuplicates list = length list == S.size (S.fromList list)

mkSimplicialSetNoDups :: [[Int]] -> Maybe SimplicialSet
mkSimplicialSetNoDups lists = do
  if not . all noDuplicates $ lists
    then Nothing
    else Just ()
  let listSimplexs = map S.fromList lists
  if not $ noDuplicates listSimplexs
    then Nothing
    else Just ()
  return $ mkSimplicialSet lists

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

simplicial2dSphere :: SimplicialSet
simplicial2dSphere = mkSimplicialSet
  [[],
  [1],[2],[3],
  [1,2],[1,3],[2,3]]

newtype SimplexsOfFixedSize = F (S.Set Simplex) deriving (Show, Eq)
splitBySizes :: SimplicialSet -> [SimplexsOfFixedSize]
splitBySizes (UnsafeSimplicialSet s) = map (F  . S.fromAscList) $ groupOn S.size $ sortOn S.size $ S.toAscList s
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
           in case addToRight prevCC matrix of
            Nothing -> error "simplicialComplex: incompatible matrices (d^2 /= 0?)"
            Just cc -> SomeChainComplex cc
      _ -> error "Malformed SomeChainComplex"

    addZeroToRight :: SomeChainComplex a -> SomeChainComplex a
    addZeroToRight (SomeChainComplex cc@(UnsafeAddToRight _ _)) = case addToRight cc (zeroMatrix @_ @0) of
      Just newCC -> SomeChainComplex newCC
      Nothing -> error "addZeroToRight  failed"
    addZeroToRight (SomeChainComplex ZeroComplex) = SomeChainComplex . fromJust $ startComplex @0 @a

simplicialHomologyOverQ :: SimplicialSet -> [Integer]
simplicialHomologyOverQ sc = case simplicialComplex sc of
  SomeChainComplex cc -> bettiNumbers cc
-- >>> simplicialHomologyOverQ simplicial2dSphere


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
-- [(Plus,fromList [2,3,4,5]),(Minus,fromList [1,3,4,5]),(Plus,fromList [1,2,4,5]),(Minus,fromList [1,2,3,5]),(Plus,fromList [1,2,3,4])]

flagN :: Int -> SimplicialSet -> SimplicialSet -- adds simplices up to dimension n with all edges present in original simplicial set
flagN n = foldr (\f g -> g . f) id (map flag' [1..n])
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

-- >>> mkSimplicialSetNoDups [[1, 2],[2, 3],[1, 3],[1, 4] ]
-- Just [[],[1],[1,2],[1,3],[1,4],[2],[2,3],[3],[4]]

-- >>> show $ flag $ testSC
-- /home/imobulus/imobulus/hse/4th-year/topan/topoll/src/Topoll/SimplicialSet.hs:(103,1)-(108,2): Non-exhaustive patterns in Just testSC
