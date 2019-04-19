{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Set5DoNotation where

import MCPrelude
import Set1RandomNumbersWithMonads (randInt, randLetter)
import Set4CommonAbstractions hiding (Monad, return)

-- 2. Do Notation – operators
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
  fail :: String -> m a
  fail = undefined

-- 3. Do Notation – Set 1
instance Monad Gen where
  (>>=) g f =
    Gen $ \s ->
      case runGen g s of
        (x, s') -> runGen (f x) s'
  return x = Gen (x, )

makeRandom = randInt

fiveRands :: Gen [Integer]
fiveRands = do
  sequence $ replicate 5 makeRandom

randString3 :: Gen String
randString3 = do
  sequence $ replicate 5 randLetter

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 = do
  x <- g1
  y <- g2
  return (x, y)
