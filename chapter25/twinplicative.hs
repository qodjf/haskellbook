-- instance types provided as they may help.

{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga



instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure . pure $ a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ liftA2 (<*>) f a


instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap h (Compose fga) = foldMap (foldMap h) fga


instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
  -- h :: a -> m b
  -- traverse h :: g a -> m (g b)
  -- traverse (traverse h) :: f (g a) -> m (f (g b))
  traverse h (Compose fga) = Compose <$> traverse (traverse h) fga
