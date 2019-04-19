{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set1RandomNumbersWithMonads where

import MCPrelude
import Set4CommonAbstractions

randInt :: Gen Integer
randInt = Gen rand

fiveRands :: [Integer]
fiveRands = evalGen (sequence $ replicate 5 randInt) (mkSeed 1)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen =
  Gen $ \s ->
    let (x, s') = runGen gen s
     in (f x, s')

randLetter :: Gen Char
randLetter = generalA toLetter randInt

randString3 :: String
randString3 = evalGen (sequence $ replicate 3 randLetter) (mkSeed 1)

randEven :: Gen Integer
randEven = generalA (* 2) randInt

randOdd :: Gen Integer
randOdd = generalA (+ 1) $ generalA (* 2) randInt

randTen :: Gen Integer
randTen = generalA (* 10) randInt

randPair :: Gen (Char, Integer)
randPair = randLetter `bind` \x -> randInt `bind` \y -> return (x, y)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 = g1 `bind` \x -> g2 `bind` \y -> return (x, y)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = liftM2 (,)
