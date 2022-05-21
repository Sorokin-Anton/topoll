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

sampleSpherePointsUniformlyAtParametrization :: Int -> IO (Vector (Vector Float))
sampleSpherePointsUniformlyAtParametrization ((<0) -> True) = fail "The sample length can't be negative"
sampleSpherePointsUniformlyAtParametrization 0 = return V.empty
sampleSpherePointsUniformlyAtParametrization numberOfPointsToSample = do
    rndGen <- newStdGen
    let (frst, rndGen') = uniformR (0 :: Float, 2 * pi) rndGen
    let (scnd, rndGen'') = uniformR (0 :: Float, 2 * pi) rndGen'
    let firstTriple = (frst, scnd, rndGen'')
    let preResult = V.iterateN numberOfPointsToSample (nextTriple (0 :: Float, 2 * pi) (0 :: Float, 2 * pi)) firstTriple
    let preResult' = preResult <&> (\(ft, nd, _) -> (ft, nd))
    return $ preResult' <&> (\(x, y) -> V.fromList [cos x * sin y, sin x * sin y, cos y])

{-
>>> unsafePerformIO (sampleSpherePointsUniformlyAtParametrization 2)
[[-0.4252499,-8.684096e-2,0.9009002],[-0.6159266,-0.7711275,0.16123512]]
-}
