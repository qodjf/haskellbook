module Main where

import First'
import MonoidLaw
import SemigroupLaw
import Test.QuickCheck
import Trivial

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivialAssoc)
