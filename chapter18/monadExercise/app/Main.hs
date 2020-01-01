module Main where

import Monads
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  let identity = undefined :: Identity (Int, String, Int)
  quickBatch $ functor identity
  quickBatch $ applicative identity
  quickBatch $ monad identity

  let phEither = undefined :: PhhhbbtttEither Int (Int, String, Int)
  quickBatch $ functor phEither
  quickBatch $ applicative phEither
  quickBatch $ monad phEither

  let nope = undefined :: Nope (Int, String, Int)
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
