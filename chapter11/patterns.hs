import Data.String
import Data.Char

upperFirst :: String -> String
upperFirst [] = []
upperFirst (x:xs) = (toUpper x) : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a1@(a:as) (b:bs) = 
  if a == b then isSubsequenceOf as bs else isSubsequenceOf a1 bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = zip ws (map upperFirst ws)
  where ws = words s
