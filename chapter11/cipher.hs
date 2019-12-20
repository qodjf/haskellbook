module Cipher where

import Data.Char

shift :: Int -> Char -> Char
shift m x = chr (((m + (ord x) - (ord 'a')) `mod` 26) + (ord 'a'))

caesar :: Int -> [Char] -> [Char]
caesar m = map (shift m)

unCaesar :: Int -> [Char] -> [Char]
unCaesar m = map (shift (26 - m))

myRepeat :: String -> String
myRepeat s = s ++ myRepeat s

vigenère :: String -> String -> String
vigenère keyWord s =
  helper (myRepeat keyWord) s
  where
    helper :: String -> String -> String
    helper _ [] = []
    helper ks (' ' : xs) = ' ' : helper ks xs
    helper (k : ks) (x : xs) = shift ((ord k) - (ord 'a')) x : helper ks xs
