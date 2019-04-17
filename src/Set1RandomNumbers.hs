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

-- 2. Random character generation
randLetter :: Seed -> (Char, Seed)
randLetter s = (toLetter x, s')
  where
    (x, s') = rand s

randString3 :: String
randString3 = take 3 $ genMany randLetter (mkSeed 1)
