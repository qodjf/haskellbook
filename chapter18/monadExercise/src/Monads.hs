module Monads where

import Prelude hiding (Left, Right)
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (EqProp, (=-=), eq)

-- Nope
data Nope a =
  NopeDotJpg deriving (Show, Eq)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

-- PhhhbbtttEither
data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Show, Eq)

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(1, Left <$> arbitrary),
                         (1, Right <$> arbitrary)]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a) = Left (f a)
  fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
  pure a = Left a
  Left f <*> Left a = Left (f a)
  Left _ <*> Right b = Right b
  Right b <*> _ = Right b

instance Monad (PhhhbbtttEither b) where
  Left a >>= f = f a
  Right b >>= _ = Right b

-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a
