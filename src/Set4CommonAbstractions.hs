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

-- 5. Revisiting Other Generic Functions
-- sequence is equivalent to repRandom
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = m `bind` \x -> sequence ms `bind` \xs -> return (x : xs)

-- liftM2 is equivalent to generalB2/yLink/allCombs
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = m1 `bind` \x -> m2 `bind` \y -> return (f x y)

-- (=<<) is equivalent to chain
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

-- join is equivalent to combine
join :: Monad m => m (m a) -> m a
join x = x `bind` id

-- liftM3 is equivalent to allCombs3
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f m1 m2 m3 =
  m1 `bind` \x -> m2 `bind` \y -> m3 `bind` \z -> return (f x y z)

-- ap is equivalent to combStep
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = mx `bind` \x -> mf `bind` \f -> return (f x)
