{-# LANGUAGE OverloadedLists #-}
module Topoll.SymplicialSet where

import Control.Monad
import Data.List (sort, sortOn, groupBy)
import qualified Data.Set as S
import Topoll.ChainComplex.Field
import qualified Data.Bifunctor
import Data.Function (on)
import Topoll.ChainComplex.Type

type Symplex  = S.Set Int

newtype SymplicialSet = UnsafeSymplicialSet (S.Set Symplex)
instance Show SymplicialSet where  show (UnsafeSymplicialSet a) = show $ map S.toAscList (S.toAscList a)

symplicialSet :: [[Int]] -> Maybe SymplicialSet
symplicialSet lists = do
  let sorted = sortOn (\x -> (length x, x)) $ map sort lists
  if containsDuplicates sorted
    then Nothing
    else UnsafeSymplicialSet <$> foldM f S.empty sorted
  where
    containsDuplicates :: Eq a => [a] -> Bool -- for sorted lists!!
    containsDuplicates xs = or $ zipWith (==) xs (tail xs)

    f :: S.Set Symplex -> [Int] -> Maybe (S.Set Symplex)
    f s sortedList =
      if containsDuplicates sortedList
        then Nothing
        else let
          symplex = S.fromList sortedList

          in if all ((`S.member` s) . snd) (symplexBorder symplex) then
            Just $ S.insert symplex s
            else Nothing

symplicial2dSphere :: SymplicialSet
Just symplicial2dSphere = symplicialSet
  [[],
  [1],[2],[3],
  [1,2],[1,3],[2,3]]

newtype SymplexsOfFixedSize = F (S.Set Symplex) deriving (Show, Eq)
splitBySizes :: SymplicialSet -> [SymplexsOfFixedSize]
splitBySizes (UnsafeSymplicialSet s) = map (F  . S.fromAscList) $ groupOn S.size $ sortOn S.size $ S.toAscList s
  where groupOn f = groupBy ((==) `on` f)

symplicialComplex :: Num a =>  SymplicialSet -> SomeChainComplex a
symplicialComplex = undefined

symplicialHomologyOverQ :: SymplicialSet -> [Integer]
symplicialHomologyOverQ s = bettiNumbers undefined
-- >>> symplicialHomologyOverQ symplicial2dSphere


data Sign = Plus | Minus deriving (Show, Eq)
reverseSign :: Sign -> Sign
reverseSign Plus = Minus
reverseSign Minus = Plus

symplexBorder :: Symplex -> [(Sign, Symplex)]
symplexBorder = map (Data.Bifunctor.second S.fromAscList) . helper Plus . S.toAscList where
  helper :: Sign -> [Int] -> [(Sign, [Int])]
  helper _ [] = []
  helper sign (x:xs) = (sign, xs) : map (Data.Bifunctor.second (x:)) (helper (reverseSign sign) xs)

-- >>> symplexBorder [1,2,3,4,5]
-- [(Plus,fromList [2,3,4,5]),(Minus,fromList [1,3,4,5]),(Plus,fromList [1,2,4,5]),(Minus,fromList [1,2,3,5]),(Plus,fromList [1,2,3,4])]
