module Topoll.DistanceMatrix.DistanceMatrix (AggregatedData(..), chooseMaxminLandmakrs, chooseRandomLandmarks) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Numeric.Natural
import System.Random
import Data.Functor

import System.IO.Unsafe (unsafePerformIO)

data AggregatedData = AggregatedData {
    landmarkPoints :: Vector (Vector Float),
    dataPoints :: Vector (Vector Float)
} deriving (Eq, Show)

euclideanMetric :: Vector Float -> Vector Float -> Float
euclideanMetric v1 v2 = sqrt $ V.sum ((V.zip v1 v2) <&> (\(x, y) -> (x - y)*(x - y)))

distToVectors :: Vector Float -> Vector (Vector Float) -> Vector Float
distToVectors vec points = points <&> (\v -> euclideanMetric v vec)

chooseMaxminLandmarksIter :: Natural -> AggregatedData -> IO AggregatedData
chooseMaxminLandmarksIter 0 points = return points
chooseMaxminLandmarksIter 1 (AggregatedData _ dpoints) = do
    let numberOfDataPoints = V.length dpoints
    gen <- newStdGen
    let (rndPos, _) = uniformR (0, numberOfDataPoints - 1) gen
    let rndDpoint = dpoints ! rndPos
    let initDpoints = V.slice 0 rndPos dpoints
    let tailDpoints = V.drop (rndPos + 1) dpoints
    return (AggregatedData (V.singleton rndDpoint) (initDpoints <> tailDpoints))
chooseMaxminLandmarksIter n dta = do
    AggregatedData landmrks dpoints <- chooseMaxminLandmarksIter (n - 1) dta
    let minDists = dpoints <&> (\x -> V.minimum (distToVectors x landmrks))
    let minmaxPos = V.maxIndex minDists
    let newLandmark = dpoints ! minmaxPos
    let initDpoints = V.slice 0 minmaxPos dpoints
    let tailDpoints = V.drop (minmaxPos + 1) dpoints
    return (AggregatedData (landmrks <> V.singleton newLandmark) (initDpoints <> tailDpoints))

{- Choose landmark points for the sample using maxmin algorithm -}
chooseMaxminLandmakrs :: Natural -> Vector (Vector Float) -> IO AggregatedData
chooseMaxminLandmakrs numberOfLandmarksToChoose samplePoints = 
    chooseMaxminLandmarksIter numberOfLandmarksToChoose (AggregatedData V.empty samplePoints)

chooseRandomLandmarksIter :: Natural -> AggregatedData -> IO AggregatedData
chooseRandomLandmarksIter 0 dta = return dta
chooseRandomLandmarksIter n dta = do
    AggregatedData landmrks dpoints <- chooseMaxminLandmarksIter (n - 1) dta
    gen <- newStdGen
    let numberOfDataPoints = V.length dpoints
    let (rndPos, _) = uniformR (0, numberOfDataPoints - 1) gen
    let newLandmark = dpoints ! rndPos
    let initDpoints = V.slice 0 rndPos dpoints
    let tailDpoints = V.drop (rndPos + 1) dpoints
    return (AggregatedData (landmrks <> V.singleton newLandmark) (initDpoints <> tailDpoints))

{- Choose landmark points from the sample uniformly at random -}
chooseRandomLandmarks :: Natural -> Vector (Vector Float) -> IO AggregatedData
chooseRandomLandmarks numberOfLandmarksToChoose samplePoints = 
    chooseRandomLandmarksIter numberOfLandmarksToChoose (AggregatedData V.empty samplePoints)

vecHelper :: [[Float]] -> Vector (Vector Float)
vecHelper lst = V.fromList (fmap V.fromList lst)

{-
>>> unsafePerformIO $ chooseRandomLandmarks 4 (vecHelper [[1,2], [3,4], [5, 6], [7, 8], [9, 10], [11, 12]])
AggregatedData {landmarkPoints = [[7.0,8.0],[1.0,2.0],[11.0,12.0],[5.0,6.0]], dataPoints = [[3.0,4.0],[9.0,10.0]]}
-}
