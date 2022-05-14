{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where
import QLinear
import Topoll.ChainComplex.Type
import Topoll.ChainComplex.Field
import Data.Ratio (Ratio)

main :: IO ()
main = do
  putStrLn $ reprChainComplexModules simpleComplex
  print $ bettiNumbers simpleComplex

simpleComplex :: ChainComplex (Ratio Integer) '[0, 5, 2, 2, 0]
Just simpleComplex =
      startComplex
      <<<
      [matrix|0 0;
              0 1|]
      <<<
      [matrix|2 0 0 0 0;
              0 0 0 0 0|]
      <<<
      zeroMatrix
