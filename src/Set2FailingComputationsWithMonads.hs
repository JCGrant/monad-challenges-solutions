{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set2FailingComputationsWithMonads where

import MCPrelude
import Set2FailingComputations hiding
  ( addSalaries
  , queryGreek
  , tailMax
  , tailMin
  , tailProd
  , tailProd2
  , tailSum
  , tailSum2
  )
import Set4CommonAbstractions

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key =
  lookupMay key greekData `bind` \xs ->
    tailMay xs `bind` \tl ->
      maximumMay tl `bind` \mx ->
        headMay xs `bind` \hd -> divMay (fromIntegral mx) (fromIntegral hd)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData name1 name2 =
  liftM2 (+) (lookupMay name1 salaryData) (lookupMay name2 salaryData)

tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `bind` \tl -> return (product tl)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = tailMay xs `bind` \tl -> return (sum tl)

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = transMaybe product . tailMay

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = transMaybe sum . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = join . transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = join . transMaybe minimumMay . tailMay
