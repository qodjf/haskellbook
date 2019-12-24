module First' where

import Optional
import Control.Monad
import Data.Monoid
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a =>
         Arbitrary (First' a) where
  arbitrary =
    frequency [(1, return (First' Nada)),
               (3, liftM (First' . Only) arbitrary)]

instance Semigroup (First' a) where
  (First' Nada) <> x = x
  x@(First' (Only a)) <> _ = x

instance Monoid (First' a) where
  mempty = First' Nada
