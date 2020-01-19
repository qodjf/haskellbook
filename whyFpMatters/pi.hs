module Pi where

addStream a b = map (uncurry (+)) (zip a b)
scaleStream s factor = map (*factor) s
partialSums (x:xs) = x : map (+x) (partialSums xs)

integersStartingFrom :: Integral t => t -> [t]
integersStartingFrom n = n : integersStartingFrom (n + 1)

ints = integersStartingFrom 1


fibGen a b = a : b : fibGen b (a + b)

fibs = fibGen 0 1

sieve :: (Integral a, Eq a) => [a] -> [a]
sieve (a:as) = a : sieve (filter (\x -> x `mod` a /= 0) as)

primes = sieve (integersStartingFrom 2)


ones = repeat 1
ints' = 1 : addStream ones ints'
fibs' = 0 : 1 : (addStream (tail fibs') fibs')

-- sqrt
sqrtImprove guess x = (guess + x / guess) / 2

sqrtStream x = guesses
  where guesses = 1.0 : map (\guess -> sqrtImprove guess x) guesses

-- pi
piSummands n = 1.0 / (fromIntegral n) : map (\x -> -x) (piSummands (n + 2))

piStream = map (*4) $ partialSums (piSummands 1)

-- accelerate
square x = x * x
eulerTransform (a:b:c:xs) =
  c - (square (c - b)) / (a - 2 * b + c) : eulerTransform (b:c:xs)

super s = map (\s -> s !! 1) (iterate eulerTransform s)

