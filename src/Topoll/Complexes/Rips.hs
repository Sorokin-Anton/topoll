module Topoll.Complexes.Rips where

import Data.Set ()
import Data.Set qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V

import Named
import Topoll.DistanceMatrix.DistanceMatrix
import Topoll.SimplicialSet

ripsEdges :: Vector Point -> ("r" :! Float) -> SimplicialSet
ripsEdges dd (Arg r) = simplicialSet ripsEdges' where
  indexedDD = V.zip (V.fromList [0, 1 .. V.length dd - 1]) dd
  ripsEdges' = V.foldl (\s (i, d) -> S.union s $ ripsEdges'' i d) S.empty indexedDD
  indexedD d = V.zip (V.fromList [0, 1 .. V.length d - 1]) d
  ripsEdges'' i d = V.foldl (\s (j, dist) -> if dist > 2 * r then s else S.insert (S.fromList [i, j]) s) S.empty (indexedD d)

{-| Given a point set, a Vietoris–Rips complex consists of all those simplices
 whose vertices are at pairwise distance no more than `r`
-}
ripsSet :: DistanceMatrix -> ("r" :! Float) -> SimplicialSet
ripsSet (DistanceMatrix dd) = flag . ripsEdges dd
