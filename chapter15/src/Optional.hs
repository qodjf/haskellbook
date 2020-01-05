module Optional where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> x = x
  x@(Only _) <> Nada = x
  (Only a) <> (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Only a) = Only $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Only a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Only a) = Only <$> f a
