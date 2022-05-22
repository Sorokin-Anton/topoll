{-# LANGUAGE DataKinds #-}

module Main where
import Topoll.SimplicialSet
import Data.Set (delete, powerSet, fromList)

main :: IO ()
main = do
  print $ simplicialHomologyOverQ $ simplicialSet (fromList [1..14] `delete` powerSet (fromList [1..14]))
