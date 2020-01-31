{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.IO (openFile, hGetContents, hClose, IOMode(..))
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']' 

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL -- important!
  return (name, val)

-- | Skip end of line and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


-- | Skip comments starting at the
-- beginning of the line.
skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
   Section h (M.fromList assignments)


rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m =
  M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

-- config directories
fromSuccess :: Result a -> a
fromSuccess (Success a) = a

parseOneFile :: FilePath -> FilePath -> IO (FilePath, Config)
parseOneFile dir file = do
  handle <- openFile (dir ++ "/" ++ file) ReadMode
  contents <- hGetContents handle
  return (file, fromSuccess $ parseString parseIni mempty contents)

parseConfigDirectories :: FilePath -> IO (Map String Config)
parseConfigDirectories dir = 
  listDirectory dir >>= fmap M.fromList . traverse (parseOneFile dir) . filter (isSuffixOf "ini")

main :: IO ()
main = do
  print $ parseByteString parseIni mempty sectionEx
  putStrLn "hello world"

  result <- parseConfigDirectories "testFiles"
  print result
