{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Topoll.ChainComplex.Field where

import QLinear ( Matrix, value )
import Topoll.ChainComplex.Type
import GHC.TypeLits (natVal)
import Data.Proxy
import qualified Numeric.Matrix as NM
import qualified Numeric.LinearAlgebra as HMatrix
import qualified Data.List



-- from bed-and-breakfast package
matrixRankOverFieldBB :: forall m n.  Matrix m n Integer -> Integer
matrixRankOverFieldBB m | all null (value m) = 0
matrixRankOverFieldBB m = NM.rank . NM.fromList . value $ m

-- from HMatrix package
matrixRankOverFieldH :: forall m n.  Matrix m n Integer -> Integer
matrixRankOverFieldH m | all null (value m) = 0
matrixRankOverFieldH m = fromIntegral $ (HMatrix.rank @Double) $ HMatrix.fromLists (map (map fromInteger) $ removeZeroRows $ value m)

removeZeroRows, removeZeroColumns :: (Eq a, Num a) => [[a]] -> [[a]]
removeZeroColumns = Data.List.transpose . removeZeroRows . Data.List.transpose -- better then traversing all lists
removeZeroRows = filter (any (/= 0))

matrixRankOverFieldNaive :: forall m n a.  (Eq a, Num a) =>  Matrix m n a -> Integer
matrixRankOverFieldNaive = f . value where



  f :: [[a]] -> Integer
  f m = case removeZeroRows $ removeZeroColumns m of
    [] -> 0
    m' -> 1 + let (a, goodRow : b) = break ((0 /=) . head) m'
              in  f $ map
                (\row -> zipWith subtract (map (*head row) goodRow) (map (*head goodRow) row))
                $ a ++ b


bettiNumbers :: ChainComplex Integer dimensions -> [Integer]
bettiNumbers cc = zipWith (-) kerDims (tail imDims) where
  imDims =  foldChainComplexPar (\(SomeMatrix m) -> matrixRankOverFieldH m) cc
  kerDims = zipWith (-)
   (foldChainComplexPar (\(SomeMatrix (_ :: Matrix m n a)) -> natVal (Proxy :: Proxy n)) cc)
   imDims
