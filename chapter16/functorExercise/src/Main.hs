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

-- EvilGoateeConst
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- LiftItOut
data LiftItOut f a =
  LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x)

-- Parappa
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

-- IgnoreOne
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

-- Notoriuos
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)

-- List
data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- GoatLord
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a1 a2 a3) = MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

-- TalkToMe
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read h) = Read (f . h)

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Identity Int))

  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (\x -> functorCompose (++"hello ") (++"world") (x :: Pair String))
