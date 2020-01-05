module S where

import Control.Applicative (liftA2)

data S n a = S (n a) a deriving (Show, Eq)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance (Traversable n) => Traversable (S n) where
  traverse f (S na a) = liftA2 S (traverse f na) (f a)
