module Main where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

testParse :: Parser () -> IO ()
testParse p = print $ parseString p mempty "123"

myString :: String -> Parser String
myString [] = pure ""
myString (x:xs) = do
  c <- char x
  cs <- myString xs
  return (c:cs)

int' :: Parser Integer
int' = do
  x <- integer
  eof
  return x

main :: IO ()
main = do
  testParse one
  print $ parseString (string "1") mempty "1"
  print $ parseString (myString "123") mempty "123"
  print $ parseString int' mempty "2341"
