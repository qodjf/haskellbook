{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Goats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42 || s > "a"

instance TooMany (Int, Int) where
  tooMany (a, b) = (a + b) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany a || tooMany b
