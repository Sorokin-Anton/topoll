{-# LANGUAGE ViewPatterns #-}

module Topoll.DistanceMatrix.DistanceMatrix where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Numeric.Natural
import System.Random

import System.IO.Unsafe (unsafePerformIO)

data AggregatedData = AggregatedData {
    landmarkPoints :: Vector (Vector Float),
    dataPoints :: Vector (Vector Float)
} deriving (Eq, Show)

chooseMinmaxLandmarksIter :: Natural -> AggregatedData -> IO AggregatedData
chooseMinmaxLandmarksIter _ ((>2).V.length . dataPoints -> False) = fail "There are less than three data points in the sample"
chooseMinmaxLandmarksIter 0 points = return points
chooseMinmaxLandmarksIter 1 (AggregatedData _ dpoints) = do
    let numberOfDpoints = V.length dpoints
    gen <- newStdGen
    let (rndPos, _) = uniformR (0 :: Int, numberOfDpoints - 1) gen
    let rndDpoint = dpoints ! rndPos
    let initDpoints = V.slice 0 rndPos dpoints
    let tailDpoints = V.drop (rndPos + 1) dpoints
    return (AggregatedData (V.singleton rndDpoint) (initDpoints <> tailDpoints))
chooseMinmaxLandmarksIter n (AggregatedData landmrks dpoints) = undefined


vecHelper :: [[Float]] -> Vector (Vector Float)
vecHelper lst = V.fromList (fmap V.fromList lst)

{-
>>> vecHelper [[1, 2, 3], [1,1,1], [2, 2, 2]]
[[1.0,2.0,3.0],[1.0,1.0,1.0],[2.0,2.0,2.0]]
-}

{-
>>> unsafePerformIO $ chooseMinmaxLandmarksIter 1 (AggregatedData V.empty (vecHelper [[1,2], [3,4], [5, 6], [7, 8], [9, 10]])) 
AggregatedData {landmarkPoints = [[5.0,6.0]], dataPoints = [[1.0,2.0],[3.0,4.0],[7.0,8.0],[9.0,10.0]]}
-}
