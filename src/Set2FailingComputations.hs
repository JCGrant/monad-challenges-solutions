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

-- 3. Chains of Failing Computations
queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key =
  case lookupMay key greekData of
    Nothing -> Nothing
    Just xs ->
      case tailMay xs of
        Nothing -> Nothing
        Just tl ->
          case maximumMay tl of
            Nothing -> Nothing
            Just mx ->
              case headMay xs of
                Nothing -> Nothing
                Just hd -> divMay (fromIntegral mx) (fromIntegral hd)

-- 4. Generalizing chains of failures
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData key =
  lookupMay key greekData `link` \xs ->
    tailMay xs `link` \tl ->
      maximumMay tl `link` \mx ->
        headMay xs `link` \hd -> divMay (fromIntegral mx) (fromIntegral hd)
