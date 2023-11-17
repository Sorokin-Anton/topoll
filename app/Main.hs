{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Set (delete, fromList, powerSet)
import Named
import Options.Generic



import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Text.Printf (printf)
import Topoll.ChainComplex.Field (bettiNumbers)
import Topoll.ChainComplex.Type
import Topoll.Complexes.Rips
import Topoll.Complexes.Witness (witnessSet)
import Topoll.DistanceMatrix.DistanceMatrix
import Topoll.Samplers.Samplers
import Topoll.SimplicialSet

data Known2dManifold = Sphere | Torus

data Args w =
    Run
      { rStart :: Maybe Float
      , rEnd :: Maybe Float
      , rStep :: Maybe Float
      , radius :: Maybe Float
      , innerRadius :: w ::: Maybe Float <?> "If specified, will sample a torus"
      , n :: w ::: Maybe Int <?> "Number of points to sample"
      , nu :: w ::: Maybe Int <?> "If specified, will create a witness complex"
      , landmarks :: w ::: Maybe Int <?> "Required for witness complex"
      , maxmin :: Bool
      , dumpPoints :: Maybe FilePath }
  | TestSphere
      {dim :: w ::: Int <?> "Dimension: 1 - circle, 2 - sphere, etc"}
  deriving Generic

instance ParseRecord (Args Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

data ToChainComplex =
  Rips Float DistanceMatrix
  | Witness Int Float DistanceMatrix
  | SimplicialSphere Int


printComplexAndHomology :: ToChainComplex -> IO ()
printComplexAndHomology s = do
  putStrLn $ case s of
    Rips r _ -> "Rips, r = " <> show r
    Witness nu r _ -> "Witness, nu = " <> show nu <> ", r = " <> show r
    SimplicialSphere n -> "Sphere, n = " <> show n
  let complex = case s of
        Rips r m -> ripsSet m ! #r r
        Witness nu r m -> case witnessSet m ! #nu nu ! #r r of
          Right c -> c
          Left msg -> error msg
        SimplicialSphere n -> simplicialSet (delete (fromList [1..n]) $ powerSet (fromList [1..n]))
  case simplicialComplex complex :: SomeChainComplex Integer of
    SomeChainComplex chainComplex -> do
      putStrLn $ reprChainComplexModules chainComplex
      print $ bettiNumbers chainComplex
  putStrLn ""

main :: IO ()
main = do
  args :: Args Unwrapped <- unwrapRecord "topoll"
  case args of
    Run {..} -> do
      pts <- case innerRadius of
        Nothing -> sampleSphereUniformly ! #r (fromMaybe 10 radius) ! #n (fromMaybe 20 n)
        Just inner -> sampleTorusUniformly
          ! #bigR (fromMaybe 10 radius)
          ! #r inner
          ! #n (fromMaybe 20 n)
      forM_ dumpPoints $ \filePath ->
        let toWolfram points =
             let f = printf "%.3f"
             in  "{" <> intercalate ","
                 (map (\v -> case V.toList v of
                  [x,y,z] -> "{" <> f x <> "," <> f y <> ", " <> f z <> "}"
                  _ -> error "toWolfram expects 3d points")
                 (V.toList points)) <> "}"
        in writeFile filePath $ toWolfram pts
      complex <-
            case nu of
              Nothing -> let m = computeTotalDistanceMatrix pts
                in return $ \r -> Rips r m
              Just nu' -> do
                dat <- computeDistanceMatrix <$> (if maxmin then chooseMaxminLandmarks else chooseRandomLandmarks)
                  ( fromIntegral $ fromMaybe (error "--landmarks is required for witness complex")
                    landmarks) pts
                return $ \r -> Witness nu' r dat
      mapM_ (printComplexAndHomology . complex) $
        let (start, step, end) = (fromMaybe 0 rStart , fromMaybe 0.5 rStep, fromMaybe 10 rEnd)
        in [start, start + step .. end]
    TestSphere n -> printComplexAndHomology (SimplicialSphere n)
