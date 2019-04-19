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

-- 4. Combinations of three things
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = allCombs3' xs ys zs
  where
    allCombs3' [] _ _ = []
    allCombs3' (x:xs) [] _ = allCombs3' xs ys zs
    allCombs3' xs (y:ys) [] = allCombs3' xs ys zs
    allCombs3' allX@(x:_) allY@(y:ys) (z:zs) = f x y z : allCombs3' allX allY zs
