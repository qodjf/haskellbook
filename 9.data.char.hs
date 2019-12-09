module Upper where
import Data.Char

upperFirst :: String -> String
upperFirst [] = []
upperFirst (x:xs) = (toUpper x) : xs

upperAll :: String -> String
upperAll [] = []
upperAll (x:xs) = (toUpper x) : (upperAll xs)

getFirstUpper :: [Char] -> Char
getFirstUpper = toUpper . head
