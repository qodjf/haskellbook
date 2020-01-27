module Main where

import Control.Applicative
import Data.Ratio ((%))
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

-- decimal or fraction
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

type DecimalOrFraction = Either Integer Rational
parseRational :: Parser DecimalOrFraction
parseRational = do
      try (Right <$> parseFraction)
  <|> (Left <$> decimal)

parseDigit :: Parser Char
parseDigit = satisfy (\c -> c >= '0' && c <= '9')

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

main :: IO ()
main = do
  testParse one
  print $ parseString (string "1") mempty "1"
  print $ parseString (myString "123") mempty "123"
  print $ parseString int' mempty "2341"

  -- parseRational
  print $ parseString parseRational mempty "123"
  print $ parseString parseRational mempty "123/10"
