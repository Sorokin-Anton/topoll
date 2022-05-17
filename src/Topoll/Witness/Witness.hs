module Topoll.Witness.Witness where

import qualified Data.Vector as V
import Data.Vector ( (!?), modify, Vector, enumFromN )
import qualified Data.Set as S
import Data.Set ( insert, union, unions, Set, powerSet, delete, intersection, elemAt )
import qualified Data.Vector.Algorithms.Merge as VA

ithSmallest :: Vector Float -> Int -> Maybe Float
ithSmallest vec i = vecSorted !? (i-1) where
  vecSorted = modify VA.sort vec

-- >>> let v = fromList [1.0, 2.0, 3.0] in ithSmallest v 1
-- Just 1.0

-- >>> let v = fromList [1.0, 2.0, 3.0] in ithSmallest v 0
-- Nothing

mi :: Vector Float -> Int -> Maybe Float
mi _ 0 = Just 0
mi di nu = ithSmallest di nu

pairs :: Ord a => [a] -> Set (a, a)
pairs [] = S.empty
pairs (a : as) = pairs as `union` pairsWith a as where
  pairsWith _ [] = S.empty
  pairsWith b (c : cs) = insert (b, c) (pairsWith c cs)

witnessEdges :: Vector (Vector Float) -> Float -> Int -> Int -> Either String (Set (Int, Int))
witnessEdges dd r nu i = do
  witnessDist <- case dd !? i of
    Nothing -> Left $ "invalid index of a witness " ++ show i
    Just v -> Right v
  m <- case mi witnessDist nu of
    Nothing -> Left "incorrect arguments to mi"
    Just mm -> Right mm
  let landmarksEnumerated = V.zip witnessDist $ enumFromN (0 :: Int) $ length witnessDist
  let closeLandmarks = V.filter ((<= m + r) . fst) landmarksEnumerated
  return . pairs . V.toList . V.map snd $ closeLandmarks

edges :: Vector (Vector Float) -> Float -> Int -> Either String (Set (Int, Int))
edges dd r nu = edges' $ take (V.length dd) $ enumFrom (0 :: Int) where
  edges' [] = return S.empty
  edges' (i : is) = do
    wEdges <- witnessEdges dd r nu i
    restEdges <- edges' is
    return $ wEdges `union` restEdges

newtype SimplicialComplex = UnsafeSimplicialComplex {
  simplices :: Set (Set Int)
} deriving Show

simplicialComplex :: Set (Set Int) -> SimplicialComplex
simplicialComplex = UnsafeSimplicialComplex . unions . S.map powerSet

vertices :: SimplicialComplex -> Set Int
vertices (UnsafeSimplicialComplex s) = unions s

neighbours :: SimplicialComplex -> Int -> Set Int
neighbours (UnsafeSimplicialComplex s) n = S.fromList $ do
  simpl <- S.toList s
  if S.size simpl == 2 && S.member n simpl
  then S.toList $ delete n simpl
  else []

flagN :: Maybe Int -> SimplicialComplex -> SimplicialComplex
flagN Nothing s = flagN (Just . S.size $ vertices s) s
flagN (Just n) s = foldr (\f g -> g . f) id (map flag' [1..n]) s
 where
  flag' :: Int -> SimplicialComplex -> SimplicialComplex
  flag' n' sc@(UnsafeSimplicialComplex s') = UnsafeSimplicialComplex  . union s' . S.fromList $ do
    nSimpl <- S.toList $ S.filter ((== n') . S.size) s'
    let allNeighbours = S.map (neighbours sc) nSimpl
    commonNeighbour <- S.toList $ foldl intersection
      (if null allNeighbours then mempty else elemAt 0 allNeighbours) allNeighbours
    return $ insert commonNeighbour nSimpl

flag :: SimplicialComplex -> SimplicialComplex
flag = flagN Nothing

witnessComplex :: Vector (Vector Float) -> Float -> Int -> Either String SimplicialComplex
witnessComplex dd r nu = do
  edg <- edges dd r nu
  let complex0 = simplicialComplex $ S.map (\(a, b) -> S.fromList [a, b]) edg
  return $ flag complex0