module Fibs where

fibs = 1 : scanl (+) 1 fibs

first20 = take 20 fibs
lessThan100 = takeWhile (<100) fibs

factorial = scanl (*) 1 [1..]
