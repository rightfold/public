module Control.Monad.Free
  ( MonadFree (..)
  , liftF
  ) where

class Monad m => MonadFree f m | m -> f where
  wrap :: f (m a) -> m a

liftF :: (Functor f, MonadFree f m) => f a -> m a
liftF = wrap . fmap pure
