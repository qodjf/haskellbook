module Main where

import List
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Tuple
import Validation

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Pair (Int, Int, Int))
  quickBatch $ applicative (undefined :: (Two String) (Int, Int, Int))
  quickBatch $ applicative (undefined :: (Validation String) (Int, Int, Int))
  quickBatch $ applicative (undefined :: ZipList' (Int, Int, Int))
  quickBatch $ monoid (undefined :: List Int)
  quickBatch $ applicative (undefined :: List (String, String, Int))
  quickBatch $ monad (undefined :: List (String, String, Int))
