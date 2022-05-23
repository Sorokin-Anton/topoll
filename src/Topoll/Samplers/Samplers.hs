{-# LANGUAGE ViewPatterns #-}

module Topoll.Samplers.Samplers
    (sampleSpherePointsUniformlyAtParametrization, 
     sampleTorusPointsUniformlyAtParametrization,
     sampleSphereUniformly) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import System.Random
import Data.Functor ( (<&>) )


nextTriple :: (Float, Float) -> (Float, Float) -> (Float, Float, StdGen) -> (Float, Float, StdGen)
nextTriple firstRange secondRange (_, _, rnd) = (firstC, fst nextPair, snd nextPair) where
    (firstC, rnd') = uniformR firstRange rnd
    nextPair = uniformR secondRange rnd'

getUniformPointsOfRectangle :: (Float, Float) -> (Float, Float) -> Int -> IO (Vector (Float, Float))
getUniformPointsOfRectangle _ _ ((<0) -> True) = fail "Can't sample negative number of points"
getUniformPointsOfRectangle firstRange secondRange numberOfPointsToSample = do
    rndGen <- newStdGen
    let (frst, rndGen') = uniformR firstRange rndGen
    let (scnd, rndGen'') = uniformR secondRange rndGen'
    let firstTriple = (frst, scnd, rndGen'')
    let preResult = V.iterateN numberOfPointsToSample (nextTriple firstRange secondRange) firstTriple
    return $ preResult <&> (\(ft, nd, _) -> (ft, nd))

sampleSpherePointsUniformlyAtParametrization :: Float -> Int -> IO (Vector (Vector Float))
sampleSpherePointsUniformlyAtParametrization _ ((<0) -> True) = fail "The sample length can't be negative"
sampleSpherePointsUniformlyAtParametrization ((<0) -> True) _ = fail "Can't sample points from the sphere of the negative radius"
sampleSpherePointsUniformlyAtParametrization _ 0 = return V.empty
sampleSpherePointsUniformlyAtParametrization sphereRadius numberOfPointsToSample = do
    preResult' <- getUniformPointsOfRectangle (0 :: Float, 2 * pi) (0 :: Float, pi) numberOfPointsToSample
    return $ preResult' <&> (\(x, y) ->
        V.fromList [sphereRadius * cos x * sin y, sphereRadius * sin x * sin y, sphereRadius * cos y])

{- First argumant is bigR, the distance from the center of the tube to the center of the torus. -}
{- The second one -- r, the radius of the tube. -}
sampleTorusPointsUniformlyAtParametrization :: Float -> Float -> Int -> IO (Vector (Vector Float))
sampleTorusPointsUniformlyAtParametrization ((<0) -> True) _ _= fail "Can't sample points from the torus with negative R"
sampleTorusPointsUniformlyAtParametrization _ ((<0) -> True) _ = fail "Can't sample points from the torus with negative r"
sampleTorusPointsUniformlyAtParametrization _ _ ((<0) -> True) = fail "The sample length can't be negative"
sampleTorusPointsUniformlyAtParametrization bigR r numberOfPointsToSample = do
    preRusult' <- getUniformPointsOfRectangle (0 :: Float, 2 * pi) (0 :: Float, 2 * pi) numberOfPointsToSample
    return $ preRusult' <&> (\(x, y) ->
        V.fromList [(bigR + r * cos x) * cos y, (bigR + r * cos x) * sin y, r * sin x])

sampleSphereUniformly :: Float -> Int -> IO (Vector (Vector Float))
sampleSphereUniformly ((<0) -> True) _= fail "Can't sample points from the sphere of the negative radius"
sampleSphereUniformly _ ((<0) -> True) = fail "The sample length can't be negative"
sampleSphereUniformly sphereRadius numberOfPointsToSample = do
    randomPoints <- getUniformPointsOfRectangle (0, 1) (0, 1) numberOfPointsToSample
    let tweakedPoints = randomPoints <&> (\(x, y) -> (2*pi*x, acos (1 - 2*y)))
    return $ tweakedPoints <&> (\(x, y) ->
        V.fromList [sphereRadius * cos x * sin y, sphereRadius * sin x * sin y, sphereRadius * cos y])

{-
>>> sampleSphereUniformly 3 3
[[1.0275645,2.4603398,1.375078],[0.5589352,-2.9438257,0.14656463],[-1.6611104,-1.6723514,-1.855789]]
-}
