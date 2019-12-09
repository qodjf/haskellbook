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
