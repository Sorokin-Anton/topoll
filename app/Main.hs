
module Main where
import Topoll.SimplicialSet
import Data.Set (delete, powerSet, fromList)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Topoll.Samplers.Samplers
import Topoll.DistanceMatrix.DistanceMatrix
import Topoll.Complexes.Rips
import Named

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      pts <- sampleSphereUniformly ! #r 10 ! #n  20
      let m = computeTotalDistanceMatrix pts
      mapM_ (\x -> print . simplicialHomologyOverQ $ ripsSet m ! #r x) [1 .. 10]
    ["sphere", readMaybe -> Just n] ->
      print $ simplicialHomologyOverQ $ simplicialSet (fromList [1..n] `delete` powerSet (fromList [1..n]))
    _ -> putStrLn "Bad args :("
