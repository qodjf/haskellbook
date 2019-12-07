module EnumFT where

eft :: Enum a => a -> a -> [a]
eft f t = map toEnum (eftInt (fromEnum f) (fromEnum t))

eftInt :: Int -> Int -> [Int]
eftInt f t
  | f > t = []
  | f == (maxBound :: Int) = [maxBound :: Int]
  | otherwise = (f : (eftInt (succ f) t))
