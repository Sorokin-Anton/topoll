{-# LANGUAGE OverloadedLists #-}
module Topoll.SimplicialSet where

import Control.Monad
import Data.List (sort, sortOn, groupBy)
import qualified Data.Set as S
import Topoll.ChainComplex.Field
import qualified Data.Bifunctor
import Data.Function (on)
import Topoll.ChainComplex.Type

type Simplex  = S.Set Int

newtype SimplicialSet = UnsafeSimplicialSet (S.Set Simplex)
instance Show SimplicialSet where  show (UnsafeSimplicialSet a) = show $ map S.toAscList (S.toAscList a)

simplicialSet :: [[Int]] -> Maybe SimplicialSet
simplicialSet lists = do
  let sorted = sortOn (\x -> (length x, x)) $ map sort lists
  if containsDuplicates sorted
    then Nothing
    else UnsafeSimplicialSet <$> foldM f S.empty sorted
  where
    containsDuplicates :: Eq a => [a] -> Bool -- for sorted lists!!
    containsDuplicates xs = or $ zipWith (==) xs (tail xs)

    f :: S.Set Simplex -> [Int] -> Maybe (S.Set Simplex)
    f s sortedList =
      if containsDuplicates sortedList
        then Nothing
        else let
          simplex = S.fromList sortedList

          in if all ((`S.member` s) . snd) (simplexBorder simplex) then
            Just $ S.insert simplex s
            else Nothing

simplicial2dSphere :: SimplicialSet
Just simplicial2dSphere = simplicialSet
  [[],
  [1],[2],[3],
  [1,2],[1,3],[2,3]]

newtype SimplexsOfFixedSize = F (S.Set Simplex) deriving (Show, Eq)
splitBySizes :: SimplicialSet -> [SimplexsOfFixedSize]
splitBySizes (UnsafeSimplicialSet s) = map (F  . S.fromAscList) $ groupOn S.size $ sortOn S.size $ S.toAscList s
  where groupOn f = groupBy ((==) `on` f)

simplicialComplex :: Num a =>  SimplicialSet -> SomeChainComplex a
simplicialComplex = undefined

simplicialHomologyOverQ :: SimplicialSet -> [Integer]
simplicialHomologyOverQ s = bettiNumbers undefined
-- >>> simplicialHomologyOverQ simplicial2dSphere


data Sign = Plus | Minus deriving (Show, Eq)
reverseSign :: Sign -> Sign
reverseSign Plus = Minus
reverseSign Minus = Plus

simplexBorder :: Simplex -> [(Sign, Simplex)]
simplexBorder = map (Data.Bifunctor.second S.fromAscList) . helper Plus . S.toAscList where
  helper :: Sign -> [Int] -> [(Sign, [Int])]
  helper _ [] = []
  helper sign (x:xs) = (sign, xs) : map (Data.Bifunctor.second (x:)) (helper (reverseSign sign) xs)

-- >>> simplexBorder [1,2,3,4,5]
-- [(Plus,fromList [2,3,4,5]),(Minus,fromList [1,3,4,5]),(Plus,fromList [1,2,4,5]),(Minus,fromList [1,2,3,5]),(Plus,fromList [1,2,3,4])]
