{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Topoll.ChainComplex.Field where

import Data.List (transpose)
import QLinear ( Matrix, value )
import Topoll.ChainComplex.Type
import Data.Ratio
import GHC.TypeLits (natVal, KnownNat)
import Data.Proxy




matrixRankOverField :: forall m n a. (Eq a, Num a) => Matrix m n a -> Integer
matrixRankOverField = f . value where

  removeZeroRows, removeZeroColumns :: [[a]] -> [[a]]
  removeZeroColumns = transpose . removeZeroRows . transpose
  removeZeroRows = filter (any (/= 0))

  f :: [[a]] -> Integer
  f m = case removeZeroRows $ removeZeroColumns m of
    [] -> 0
    m' -> 1 + let (a, goodRow : b) = break ((0 /=) . head) m'
              in  f $ map
                (\row -> zipWith subtract (map (*head row) goodRow) (map (*head goodRow) row))
                $ a ++ b

imageDimension :: forall m n a. (Eq a, Num a) => Matrix m n a -> Integer
imageDimension = matrixRankOverField

kernelDimension :: forall m n a. (Eq a, Num a, KnownNat n) => Matrix m n a -> Integer
kernelDimension m = natVal (Proxy :: Proxy n) - matrixRankOverField m

bettiNumbers :: ChainComplex (Ratio Integer) dimensions -> [Integer]
bettiNumbers ZeroComplex = []
bettiNumbers (UnsafeAddToRight ZeroComplex _) = []
bettiNumbers (UnsafeAddToRight rest@(UnsafeAddToRight _ m1) m2) = bettiNumbers rest ++ [kernelDimension m1 - imageDimension m2]
