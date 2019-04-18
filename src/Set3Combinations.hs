{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3Combinations where

import MCPrelude

-- 1. Generating combinations
allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,)

-- 2. Poker hands
data Card =
  Card Int
       String

instance Show Card where
  show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

-- 3. Generalizing pairs and cards
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f xs ys = allCombs' xs ys
  where
    allCombs' [] _ = []
    allCombs' (x:xs) [] = allCombs' xs ys
    allCombs' allX@(x:_) (y:ys) = f x y : allCombs' allX ys
