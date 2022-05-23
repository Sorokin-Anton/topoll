module Topoll.Complexes.Rips where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import qualified Data.Set as S
import Data.Set ()

import Topoll.SimplicialSet

ripsEdges :: Vector (Vector Float) -> Float -> SimplicialSet
ripsEdges dd r = simplicialSet ripsEdges' where
  indexedDD = V.zip (V.fromList [0, 1 .. V.length dd - 1]) dd
  ripsEdges' = V.foldl (\s (i, d) -> S.union s $ ripsEdges'' i d) S.empty indexedDD
  indexedD d = V.zip (V.fromList [0, 1 .. V.length d - 1]) d
  ripsEdges'' i d = V.foldl (\s (j, dist) -> if dist > 2 * r then s else S.insert (S.fromList [i, j]) s) S.empty (indexedD d)

ripsSet :: Vector (Vector Float) -> Float -> SimplicialSet
ripsSet dd r = flag $ ripsEdges dd r

-- TEST

distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)

distanceMatrix :: [(Float, Float)] -> [(Float, Float)] -> Vector (Vector Float)
distanceMatrix dataPoints landmarkPoints = V.fromList . map V.fromList $ distanceMatrixList dataPoints landmarkPoints where
  distanceMatrixList [] _ = []
  distanceMatrixList (p : ps) lPoints = map (distance p) lPoints : distanceMatrixList ps lPoints

testDataPoints :: [(Float, Float)]
testDataPoints = [(1, -1), (-1, 1), (2, 2)]

-- >>> ripsSet (distanceMatrix testDataPoints testDataPoints) 1.5
-- [[],[0],[0,1],[1],[2]]

