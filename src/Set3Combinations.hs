{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3Combinations where

import MCPrelude

-- 1. Generating combinations
allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs ys = allPairs' xs ys
  where
    allPairs' [] _ = []
    allPairs' (x:xs) [] = allPairs' xs ys
    allPairs' allX@(x:_) (y:ys) = (x, y) : allPairs' allX ys

-- 2. Poker hands
data Card =
  Card Int
       String

instance Show Card where
  show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards xs ys = allCards' xs ys
  where
    allCards' [] _ = []
    allCards' (x:xs) [] = allCards' xs ys
    allCards' allX@(x:_) (y:ys) = Card x y : allCards' allX ys
