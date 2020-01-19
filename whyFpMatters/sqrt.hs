module Sqrt where

next :: Fractional a => a -> a -> a
next n x = (x + n / x) / 2

within :: (Ord a, Num a) => a -> [a] -> a
within eps (a:b:xs)
  | abs (a - b) <= eps = b
  | otherwise = within eps (b:xs)

relative :: (Ord a, Num a) => a -> [a] -> a
relative eps (a:b:xs)
  | abs (a - b) <= eps * abs b = b
  | otherwise = relative eps (b:xs)

sqrt :: (Ord a, Fractional a) => a -> a -> a -> a
sqrt a0 eps n = relative eps (iterate (next n) a0)
