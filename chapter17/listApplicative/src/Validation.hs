module Validation where

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (EqProp, (=-=), eq)

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [(1, Failure <$> arbitrary),
               (3, Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  Failure e1 =-= Failure e2 = eq e1 e2
  Success a1 =-= Success a2 = eq a2 a2
  _ =-= _ = (eq 1 2)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e =>
         Applicative (Validation e) where
  pure  = Success
  Failure e1 <*> Failure e2 = Failure $ e1 <> e2
  Failure e <*> Success _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)
