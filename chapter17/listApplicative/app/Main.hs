module Main where

import List
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: ZipList' (Int, Int, Int))
  quickBatch $ monoid (undefined :: List Int)
  quickBatch $ applicative (undefined :: List (String, String, Int))
