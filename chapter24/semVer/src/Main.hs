module Main where

import Control.Applicative
import Text.Read (readMaybe)
import Text.Trifecta

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 _ _) (SemVer major2 minor2 patch2 _ _) =
    case (take 1 $ filter (/= EQ) [compare major1 major2, compare minor1 minor2, compare patch1 patch2]) of
      [] -> EQ
      x:xs -> x

parseStr :: Parser String
parseStr = some (noneOf ".-+")

parseNumOrString :: Parser NumberOrString
parseNumOrString = fmap (\s -> case readMaybe s of
  Just a -> NOSI a
  Nothing -> NOSS s) parseStr

parseNumOrStringList :: Parser [NumberOrString]
parseNumOrStringList = (:) <$> parseNumOrString <*> many (char '.' *> parseNumOrString)

parseRelease :: Parser [NumberOrString]
parseRelease = (char '-' *> parseNumOrStringList) <|> pure []

parseMetaData :: Parser [NumberOrString]
parseMetaData = (char '+' *> parseNumOrStringList) <|> pure []

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> integer <* char '.' <*> integer <* char '.' <*> integer <*> parseRelease <*> parseMetaData <* eof

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn . show $ parseString parseSemVer mempty "2.1.1"
  putStrLn . show $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  putStrLn . show $ parseString parseSemVer mempty "1.0.0-x.7.z.92+exp.sha.5114f85"
  putStrLn . show $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
