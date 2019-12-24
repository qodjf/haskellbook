module Mem where

import Data.Monoid

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) =
    Mem (\s ->
           let (a1, s1) = f s
               (a2, s2) = g s1
               in (a1 <> a2, s2))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
