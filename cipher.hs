module Cipher where

import Data.Char

shift :: Int -> Char -> Char
shift m x = chr (((m + (ord x) - (ord 'a')) `mod` 26) + (ord 'a'))

caesar :: Int -> [Char] -> [Char]
caesar m = map (shift m)

unCaesar :: Int -> [Char] -> [Char]
unCaesar m = map (shift (26 - m))
