lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f :: Either a b -> [a] -> [a]
    f (Left a) as = a : as
    f _ as = as

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f :: Either a b -> [b] -> [b]
    f (Right b) bs = b : bs
    f _ bs = bs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where
    f :: Either a b -> ([a], [b]) -> ([a], [b])
    f (Left a) (as, bs) = (a : as, bs)
    f (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (\_ -> Nothing) (\b -> Just (g b))
