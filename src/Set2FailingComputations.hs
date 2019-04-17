{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set2FailingComputations where

import MCPrelude

-- 1. The Maybe Type
data Maybe a
  = Nothing
  | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x

instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

instance Ord a => Ord (Maybe a) where
  Just x <= Just y = x <= y

-- 2. Build a library of things that can fail
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay key ((k, v):kvs)
  | key == k = Just v
  | otherwise = lookupMay key kvs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ foldl1 max xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just $ foldl1 min xs
