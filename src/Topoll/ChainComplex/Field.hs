{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Topoll.ChainComplex.Field where

import QLinear ( Matrix, value )
import Topoll.ChainComplex.Type
import GHC.TypeLits (natVal)
import Data.Proxy
import qualified Numeric.Matrix as NM
import qualified Data.List



matrixRankOverField :: forall m n.  Matrix m n Integer -> Integer
matrixRankOverField m | all null (value m) = 0
matrixRankOverField m = NM.rank . NM.fromList . value $ m


matrixRankOverFieldNaive :: forall m n a.  (Eq a, Num a) =>  Matrix m n a -> Integer
matrixRankOverFieldNaive = f . value where

  removeZeroRows, removeZeroColumns :: [[a]] -> [[a]]
  removeZeroColumns = Data.List.transpose . removeZeroRows . Data.List.transpose
  removeZeroRows = filter (any (/= 0))

  f :: [[a]] -> Integer
  f m = case removeZeroRows $ removeZeroColumns m of
    [] -> 0
    m' -> 1 + let (a, goodRow : b) = break ((0 /=) . head) m'
              in  f $ map
                (\row -> zipWith subtract (map (*head row) goodRow) (map (*head goodRow) row))
                $ a ++ b


bettiNumbers :: ChainComplex Integer dimensions -> [Integer]
bettiNumbers cc = zipWith (-) kerDims (tail imDims) where
  imDims =  foldChainComplexPar (\(SomeMatrix m) -> matrixRankOverField m) cc
  kerDims = zipWith (-)
   (foldChainComplexPar (\(SomeMatrix (_ :: Matrix m n a)) -> natVal (Proxy :: Proxy n)) cc)
   imDims
