{-# LANGUAGE InstanceSigs #-}
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) = StateT $ \s ->
    fmap (\(a,s1) -> (f a, s1)) (smas s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smfs) <*> (StateT smas) = StateT $ \s -> do
    (f, s1) <- smfs s
    (a, s2) <- smas s1
    return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f = StateT $ \s -> do
    (a, s1) <- sma s
    runStateT (f a) s1

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
