{-# LANGUAGE ViewPatterns #-}

module Topoll.Samplers.Samplers(sampleSpherePointsUniformlyAtParametrization) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import System.Random
import Data.Functor ( (<&>) )

import System.IO.Unsafe

nextTriple :: (Float, Float) -> (Float, Float) -> (Float, Float, StdGen) -> (Float, Float, StdGen)
nextTriple firstRange secondRange (_, _, rnd) = (firstC, fst nextPair, snd nextPair) where
    (firstC, rnd') = uniformR firstRange rnd
    nextPair = uniformR secondRange rnd'

sampleSpherePointsUniformlyAtParametrization :: Float -> Int -> IO (Vector (Vector Float))
sampleSpherePointsUniformlyAtParametrization _ ((<0) -> True) = fail "The sample length can't be negative"
sampleSpherePointsUniformlyAtParametrization ((<0) -> True) _ = fail "Can't sample points from the sphere of the negative radius"
sampleSpherePointsUniformlyAtParametrization _ 0 = return V.empty
sampleSpherePointsUniformlyAtParametrization sphereRadius numberOfPointsToSample = do
    rndGen <- newStdGen
    let (frst, rndGen') = uniformR (0 :: Float, 2 * pi) rndGen
    let (scnd, rndGen'') = uniformR (0 :: Float, 2 * pi) rndGen'
    let firstTriple = (frst, scnd, rndGen'')
    let preResult = V.iterateN numberOfPointsToSample (nextTriple (0 :: Float, 2 * pi) (0 :: Float, 2 * pi)) firstTriple
    let preResult' = preResult <&> (\(ft, nd, _) -> (ft, nd))
    return $ preResult' <&> (\(x, y) -> 
        V.fromList [sphereRadius * cos x * sin y, sphereRadius * sin x * sin y, sphereRadius * cos y])

{-
>>> unsafePerformIO (sampleSpherePointsUniformlyAtParametrization (-3) 2)
user error (Can't sample points from the sphere of the negative radius)
-}
