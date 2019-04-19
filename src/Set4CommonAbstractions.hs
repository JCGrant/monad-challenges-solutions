{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4CommonAbstractions where

import MCPrelude

-- 1. Generalizing State and Maybe
-- genTwo, link
--   :: m a -> (a -> m b) -> m b

-- generalB, yLink
--   :: (a -> b -> c) -> m a -> m b -> m c
