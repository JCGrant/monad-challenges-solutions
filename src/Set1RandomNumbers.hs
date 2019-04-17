{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set1RandomNumbers where

import MCPrelude

-- 1. Random number generation
fiveRands :: [Integer]
fiveRands = take 5 $ genMany rand (mkSeed 1)

genMany :: (Seed -> (a, Seed)) -> Seed -> [a]
genMany rand s = x : genMany rand s'
  where
    (x, s') = rand s
