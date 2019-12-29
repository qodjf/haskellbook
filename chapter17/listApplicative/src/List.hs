module List where

import Data.Semigroup

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil l = l
  (<>) (Cons a as) l = Cons a (as <> l)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> l = Nil
  (Cons f fs) <*> l = (fmap f l) <> (fs <*> l)
