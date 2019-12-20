import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (' ':xs) = ' ' : capitalizeWord xs
capitalizeWord (x:xs) = (toUpper x) : xs

split :: Char -> String -> [String]
split c = g . dropWhile (==c)
  where
      g [] = []
      g s = (takeWhile (/=c) s) : (g . dropWhile (==c) . dropWhile (/=c) $ s)

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . map capitalizeWord . split '.'
