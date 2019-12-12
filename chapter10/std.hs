module Standard where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a b -> a == e || b) False

myElem' e = myAny (== e)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\a b -> if pred a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = foldl (\b a -> if comp b a == GT then b else a) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = foldl (\b a -> if comp b a == LT then b else a) x xs
