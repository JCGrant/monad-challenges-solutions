{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Set5DoNotation where

import MCPrelude
import Set1RandomNumbersWithMonads (randInt, randLetter)
import Set2FailingComputations hiding
  ( addSalaries
  , queryGreek
  , tailMax
  , tailProd
  , tailSum
  )
import Set3Combinations hiding (allCards, allCombs3, allPairs)
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

-- 4. Do Notation – Set 2
instance Monad Maybe where
  (>>=) Nothing _ = Nothing
  (>>=) (Just x) f = f x
  return = Just

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key = do
  xs <- lookupMay key greekData
  tl <- tailMay xs
  mx <- maximumMay tl
  hd <- headMay xs
  divMay (fromIntegral mx) (fromIntegral hd)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData name1 name2 = do
  s1 <- lookupMay name1 salaryData
  s2 <- lookupMay name2 salaryData
  return (s1 + s2)

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
  tl <- tailMay xs
  return $ product tl

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
  tl <- tailMay xs
  return $ sum tl

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
  tl <- tailMay xs
  minimumMay tl

-- 5. Do Notation – Set 3
instance Monad [] where
  xs >>= f = concatMap f xs
  return x = [x]

allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

allPairs2 :: [a] -> [b] -> [(a, b)]
allPairs2 xs ys = [(x, y) | x <- xs, y <- ys]

allCards :: [Int] -> [String] -> [Card]
allCards xs ys = do
  x <- xs
  y <- ys
  return $ Card x y

allCards2 :: [Int] -> [String] -> [Card]
allCards2 xs ys = [Card x y | x <- xs, y <- ys]

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = do
  x <- xs
  y <- ys
  z <- zs
  return $ f x y z

allCombs3_2 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3_2 f xs ys zs = [f x y z | x <- xs, y <- ys, z <- zs]
