module Topoll.Complexes.Rips where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import qualified Data.Set as S
import Data.Set ()

import Topoll.SimplicialSet
import Topoll.DistanceMatrix.DistanceMatrix
import Named

ripsEdges :: Vector Point -> ("r" :! Float) -> SimplicialSet
ripsEdges dd (Arg r) = simplicialSet ripsEdges' where
  indexedDD = V.zip (V.fromList [0, 1 .. V.length dd - 1]) dd
  ripsEdges' = V.foldl (\s (i, d) -> S.union s $ ripsEdges'' i d) S.empty indexedDD
  indexedD d = V.zip (V.fromList [0, 1 .. V.length d - 1]) d
  ripsEdges'' i d = V.foldl (\s (j, dist) -> if dist > 2 * r then s else S.insert (S.fromList [i, j]) s) S.empty (indexedD d)

ripsSet :: DistanceMatrix -> ("r" :! Float) -> SimplicialSet
ripsSet dd = flag . ripsEdges dd
