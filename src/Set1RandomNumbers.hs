{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set1RandomNumbers where

import MCPrelude

-- 1. Random number generation
fiveRands :: [Integer]
fiveRands = take 5 $ genMany rand (mkSeed 1)

genMany :: Gen a -> Seed -> [a]
genMany rand s = x : genMany rand s'
  where
    (x, s') = rand s

-- 2. Random character generation
randLetter :: Gen Char
randLetter s = (toLetter x, s')
  where
    (x, s') = rand s

randString3 :: String
randString3 = take 3 $ genMany randLetter (mkSeed 1)

-- 3. More Generators
type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen s = (f x, s')
  where
    (x, s') = gen s

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (+ 1) $ generalA (* 2) rand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10) rand
