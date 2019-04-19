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
allCombs f xs ys = map f xs `combStep` ys

-- 4. Combinations of three things
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = map f xs `combStep` ys `combStep` zs

-- 5. Combinations of more things
combStep :: [a -> b] -> [a] -> [b]
combStep fs xs = combStep' fs xs
  where
    combStep' [] _ = []
    combStep' (f:fs) [] = combStep' fs xs
    combStep' allF@(f:fs) (x:xs) = f x : combStep' allF xs
