module List where

import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toMyList <$> arbitrary

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance Semigroup (List a) where
  (<>) = append

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  fs <*> as = flatMap (\f -> fmap f as) fs

instance Monad List where
  la >>= f = flatMap f la

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' x (Cons a as) = Cons a (take' (x - 1) as)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipMyList :: List (a -> b) -> List a -> List b
zipMyList Nil _ = Nil
zipMyList _ Nil = Nil
zipMyList (Cons f fs) (Cons a as) = Cons (f a) (zipMyList fs as)

instance Applicative ZipList' where
  pure = ZipList' . toMyList . repeat
  (ZipList' fs) <*> (ZipList' as) = ZipList' $ zipMyList fs as
