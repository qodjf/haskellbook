import Prelude hiding (Either, Left, Right)

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data Either a b =
    Left a
  | Right b
instance Bifunctor Either where
  bimap f g (Left a) = Left $ f a
  bimap f g (Right b) = Right $ g b
