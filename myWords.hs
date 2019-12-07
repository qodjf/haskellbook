module MyWords where

myWords :: String -> [String]
myWords = g . dropWhile (==' ')
    where
        g [] = []
        g s = (takeWhile (/=' ') s) : (g . dropWhile (==' ') . dropWhile (/=' ') $ s)
