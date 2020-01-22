module Main where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

testParse :: Parser () -> IO ()
testParse p = print $ parseString p mempty "123"

main :: IO ()
main = do
  testParse one
