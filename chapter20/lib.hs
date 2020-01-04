import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . Prelude.foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . Prelude.foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = foldr (\e b -> b || e == a) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where
    f :: (Ord a) => a -> Maybe a -> Maybe a
    f a Nothing = Just a
    f a (Just a1) = Just $ min a a1

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where
    f :: (Ord a) => a -> Maybe a -> Maybe a
    f a Nothing = Just a
    f a (Just a1) = Just $ max a a1

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ c -> c + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = Prelude.foldMap id

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\a m -> mappend (f a) m) mempty
