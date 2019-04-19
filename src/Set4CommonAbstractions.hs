{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4CommonAbstractions where

import MCPrelude

-- 3. Formalizing the Pattern
class Monad m
  -- bind is equivalent to genTwo/link
  where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- liftM2 is equivalent to generalB2/yLink
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = m1 `bind` \x -> m2 `bind` \y -> return (f x y)
