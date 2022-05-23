module Topoll.Complexes.Witness where

import qualified Data.Vector as V
import Data.Vector ( (!?), modify, Vector, enumFromN )
import qualified Data.Set as S
import Data.Set ( insert, union, Set )
import qualified Data.Vector.Algorithms.Merge as VA

import Topoll.SimplicialSet
import Topoll.DistanceMatrix.DistanceMatrix
import Named

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


edges :: DistanceMatrix -> ("r" :! Float) -> ("nu" :! Int) -> Either String (Set (Int, Int))
edges dd (Arg r) (Arg nu) = edges' $ take (V.length dd) $ enumFrom (0 :: Int) where
  edges' [] = return S.empty
  edges' (i : is) = do
    wEdges <- witnessEdges i
    restEdges <- edges' is
    return $ wEdges `union` restEdges

  witnessEdges :: Int -> Either String (Set (Int, Int))
  witnessEdges i = do
    witnessDist <- case dd !? i of
      Nothing -> Left $ "invalid index of a witness " ++ show i
      Just v -> Right v
    m <- case mi witnessDist nu of
      Nothing -> Left "incorrect arguments to mi"
      Just mm -> Right mm
    let landmarksEnumerated = V.zip witnessDist $ enumFromN (0 :: Int) $ length witnessDist
    let closeLandmarks = V.filter ((<= m + r) . fst) landmarksEnumerated
    return . pairs . V.toList . V.map snd $ closeLandmarks


witnessSet :: DistanceMatrix -> ("r" :! Float) -> ("nu" :! Int) -> Either String SimplicialSet
witnessSet dd r nu = do
  edg <- edges dd r nu
  let allEdges = S.map (\(a, b) -> S.fromList [a, b]) edg
  let allVertices = case dd !? 0 of
        Nothing -> S.empty :: Set (Set Int)
        Just v -> S.fromList . map S.singleton . take (V.length v) $ [0, 1 ..]
  let complex0 = simplicialSet $ S.union allEdges allVertices
  return $ flag complex0
