{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Topoll.ChainComplex.Type where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import QLinear (Matrix, value, (~*~))
import Data.Ratio
import Internal.Matrix (Matrix(..))
import Control.Parallel.Strategies (parMap, NFData, rdeepseq)


data ChainComplex (a :: Type) (dimensions :: [Nat]) where -- chain complex of f.g. free modules over `a`, first element in dimension list is highest grade
  ZeroComplex :: ChainComplex a '[0]
  UnsafeAddToRight :: (KnownNat x, KnownNat y) => ChainComplex a (x ': xs) -> Matrix x y a -> ChainComplex a (y ': x ': xs) -- inductively adds `a^y` to right

foldChainComplexPar :: NFData b => (SomeMatrix a -> b) -> ChainComplex a dims -> [b]
foldChainComplexPar f = parMap rdeepseq   f . reverse . collectMatrixes  where
  collectMatrixes :: ChainComplex a d -> [SomeMatrix a]
  collectMatrixes ZeroComplex = []
  collectMatrixes (UnsafeAddToRight cc m) = SomeMatrix m : collectMatrixes cc

data SomeMatrix a where
  SomeMatrix :: (KnownNat m, KnownNat n) => Matrix m n a -> SomeMatrix a

data SomeChainComplex a where
  SomeChainComplex :: ChainComplex a dims -> SomeChainComplex a

addToRight :: (Num a, Eq a, KnownNat y) => ChainComplex a (x ': xs) -> Matrix x y a -> Maybe (ChainComplex a (y ': x ': xs))
addToRight ZeroComplex m = Just (UnsafeAddToRight ZeroComplex m)
addToRight old@(UnsafeAddToRight _ m1) m2 | all (all (== 0)) $ value (m1 ~*~ m2) = Just (UnsafeAddToRight old m2)
addToRight _ _ = Nothing

infixl 3 <<<
(<<<) :: (Num a, Eq a, KnownNat y) => Maybe (ChainComplex a (x : xs)) -> Matrix x y a -> Maybe (ChainComplex a (y : x : xs))
Nothing <<< _ = Nothing
Just a <<< b = a `addToRight` b

class (Show a, Num a) => KnownBaseRing a where
  reprBaseRing :: Proxy a -> String

  reprFreeModule :: Proxy a -> Integer -> String
  reprFreeModule a n = case n of
    0 -> "0"
    1 -> reprBaseRing a
    _ -> reprBaseRing a <> "^" <> show n

reprChainComplexModules :: KnownBaseRing a => ChainComplex a dimensions -> String
reprChainComplexModules ZeroComplex = "0"
reprChainComplexModules (UnsafeAddToRight rest (_ :: Matrix p q a)) = reprChainComplexModules rest <> " <- " <> reprFreeModule (Proxy @a) (natVal $ Proxy @q)

instance KnownBaseRing Integer where
  reprBaseRing _ = "ℤ"

instance KnownBaseRing (Ratio Integer) where
  reprBaseRing _ = "ℚ"

zeroMatrix :: forall m n a. (KnownNat m, KnownNat n, Num a) => Matrix m n a
zeroMatrix = Matrix (m, n) . replicate m $ replicate n 0
  where m = fromInteger $ natVal (Proxy @m)
        n = fromInteger $ natVal (Proxy @n)

startComplex :: (KnownNat n, Num a, Eq a) => Maybe (ChainComplex a '[n, 0])
startComplex = Just ZeroComplex <<< zeroMatrix
