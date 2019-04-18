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
