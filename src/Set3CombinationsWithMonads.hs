{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3CombinationsWithMonads where

import MCPrelude
import qualified Set3Combinations as C
import Set4CommonAbstractions

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = liftM2 (,)

allCards :: [Int] -> [String] -> [C.Card]
allCards = liftM2 C.Card
