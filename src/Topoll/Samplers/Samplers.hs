{-# LANGUAGE ViewPatterns #-}

module Topoll.Samplers.Samplers(sampleSpherePointsUniformlyAtParametrization, sampleTorusPointsUniformlyAtParametrization) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import System.Random
import Data.Functor ( (<&>) )

import System.IO.Unsafe

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
    preResult' <- getUniformPointsOfRectangle (0 :: Float, 2 * pi) (0 :: Float, 2 * pi) numberOfPointsToSample
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

{-
>>> unsafePerformIO (sampleTorusPointsUniformlyAtParametrization 3 1 3)
[[-1.0837613,-2.9714236,0.9866436],[-0.72174066,2.1558383,0.68710744],[-1.64129,2.131897,-0.95090103]]
-}
