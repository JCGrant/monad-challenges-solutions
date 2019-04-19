{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Set4CommonAbstractions where

import MCPrelude
import Set2FailingComputations

-- 3. Formalizing the Pattern
class Monad m
  -- bind is equivalent to genTwo/link
  where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- liftM2 is equivalent to generalB2/yLink
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = m1 `bind` \x -> m2 `bind` \y -> return (f x y)

-- 4. Creating Instances
newtype Gen a = Gen
  { runGen :: Seed -> (a, Seed)
  }

evalGen :: Gen a -> Seed -> a
evalGen g s = fst $ runGen g s

instance Monad Gen where
  bind g f =
    Gen $ \s ->
      case runGen g s of
        (x, s') -> runGen (f x) s'
  return x = Gen (x, )

instance Monad Maybe where
  bind Nothing f = Nothing
  bind (Just x) f = f x
  return = Just

instance Monad [] where
  bind = flip concatMap
  return x = [x]
