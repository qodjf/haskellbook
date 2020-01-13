import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start end = fizzbuzzList $ [end,(end - 1)..start]

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = do
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
  putStrLn $ replicate 30 '-'
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
