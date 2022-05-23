module Topoll.DistanceMatrix.DistanceMatrix
    (Point, DistanceMatrix, AggregatedData(..), chooseMaxminLandmarks, chooseRandomLandmarks, computeDistanceMatrix, computeTotalDistanceMatrix) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Numeric.Natural
import System.Random
import Data.Functor

type Point = Vector Float
type DistanceMatrix = Vector (Vector Float)


data AggregatedData = AggregatedData {
    landmarkPoints :: Vector Point,
    dataPoints :: Vector Point
} deriving (Eq, Show)

euclideanMetric :: Vector Float -> Vector Float -> Float
euclideanMetric v1 v2 = sqrt $ V.sum (V.zip v1 v2 <&> (\(x, y) -> (x - y)*(x - y)))

distToVectors :: Vector Float -> Vector (Vector Float) -> Vector Float
distToVectors vec points = euclideanMetric vec <$> points

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
chooseMaxminLandmarks :: Natural -> Vector Point -> IO AggregatedData
chooseMaxminLandmarks numberOfLandmarksToChoose samplePoints =
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
chooseRandomLandmarks :: Natural -> Vector Point -> IO AggregatedData
chooseRandomLandmarks numberOfLandmarksToChoose samplePoints =
    chooseRandomLandmarksIter numberOfLandmarksToChoose (AggregatedData V.empty samplePoints)

{- Compute distance matrix from the aggregated data (landmarks and data points) -}
computeDistanceMatrix :: AggregatedData -> DistanceMatrix
computeDistanceMatrix (AggregatedData landmarks dpoints) =
    (`distToVectors` landmarks) <$> dpoints

{- Compute distances between all points -}
computeTotalDistanceMatrix :: Vector Point -> DistanceMatrix
computeTotalDistanceMatrix points = computeDistanceMatrix (AggregatedData points points)
