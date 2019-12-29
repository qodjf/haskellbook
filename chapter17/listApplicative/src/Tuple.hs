module Tuple where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Pair a = Pair a a deriving (Eq, Show)

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  Pair f1 f2 <*> Pair a1 a2 = Pair (f1 a1) (f2 a2)

data Two a b = Two a b deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure b = Two mempty b
  Two a1 f <*> Two a2 b = Two (a1 <> a2) (f b)
