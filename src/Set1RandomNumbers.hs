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

-- 4. Generalizing Random Pairs
randPair :: Gen (Char, Integer)
randPair s = ((c, i), s'')
  where
    (c, s') = randLetter s
    (i, s'') = rand s'

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 s = ((x, y), s'')
  where
    (x, s') = g1 s
    (y, s'') = g2 s'

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 s = (f x y, s'')
  where
    (x, s') = g1 s
    (y, s'') = g2 s'

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (\x y -> (x, y))

-- 5. Generalizing Lists of Generators
repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom (g:gs) s = (x : xs, s'')
  where
    (x, s') = g s
    (xs, s'') = repRandom gs s'

-- 6. Threading the random number state
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s = (x', s'')
  where
    (x, s') = g s
    (x', s'') = f x s'

mkGen :: a -> Gen a
mkGen x s = (x, s)
