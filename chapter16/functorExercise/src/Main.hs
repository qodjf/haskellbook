{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) =>
                        f a
                     -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g . fmap f $ x) == (fmap (g . f) x)

-- Identity
newtype Identity a = Identity a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- Pair
data Pair a = Pair a a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return (Pair a1 a2)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- FlipFunctor
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Identity Int))

  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (\x -> functorCompose (++"hello ") (++"world") (x :: Pair String))
