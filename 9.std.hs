module Standard where

-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem ele (a:as) = if ele == a then True else myElem ele as

myReverse :: [a] -> [a]
myReverse = g []
  where
    g :: [a] -> [a] -> [a]
    g acc [] = acc
    g acc (a:as) = g (a:acc) as

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ (squish ls)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (l:ls) = f l ++ (squishMap f ls)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = helper comp xs x
  where
    helper :: (a -> a -> Ordering) -> [a] -> a -> a
    helper _ [] a = a
    helper comp (x:xs) a =
      helper comp xs (case comp a x of
        LT -> x
        otherwise -> a)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = helper comp xs x
  where
    helper :: (a -> a -> Ordering) -> [a] -> a -> a
    helper _ [] a = a
    helper comp (x:xs) a =
      helper comp xs (case comp a x of
        GT -> x
        otherwise -> a)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
