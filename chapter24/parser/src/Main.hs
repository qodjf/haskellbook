module Main where

import Control.Applicative
import Data.Char
import Data.Ratio ((%))
import Data.Word
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

base10Integer' :: Parser Integer
base10Integer' = read <$>
      (((:) <$> char '-' <*> some parseDigit)
   <|> (some parseDigit))

-- parse phone number
type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseInt :: Parser Int
parseInt = fromInteger <$> integer

parsePhone :: Parser PhoneNumber
parsePhone =
      try (PhoneNumber <$> parseInt <* char '-' <*> parseInt <* char '-' <*> parseInt <* eof)
  <|> try (PhoneNumber <$> (parseInt *> char '-' *> parseInt <* char '-') <*> parseInt <* char '-' <*> parseInt <* eof)
  <|> (PhoneNumber <$> (char '(' *> parseInt <* string ") ") <*> parseInt <* char '-' <*> parseInt)
  <|> (PhoneNumber <$> (read <$> count 3 digit) <*> (read <$> count 3 digit) <*> parseInt)

-- parse Ipv4
data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseWord32 :: Parser Word32
parseWord32 = read <$> some (noneOf ".")

calculateIpv4 :: Word32 -> Word32 -> Word32 -> Word32 -> IPAddress
calculateIpv4 a b c d = IPAddress $ ((a * 256 + b) * 256 + c) * 256 + d

parseIpv4 :: Parser IPAddress
parseIpv4 = calculateIpv4 <$> (parseWord32 <* char '.') <*> (parseWord32 <* char '.') <*> (parseWord32 <* char '.') <*> (parseWord32 <* eof)

-- parse IPv6
data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

acc :: Int -> Char -> Int
acc a c = a * 16 + digitToInt c

parseSeg :: Parser String
parseSeg = many (noneOf ":")

parseSegs :: Parser [String]
parseSegs = sepBy parseSeg (char ':')

fillSegs :: [String] -> [String]
fillSegs segs = case 8 - length segs of
  0 -> segs
  d -> g d segs
  where
    g :: Int -> [String] -> [String]
    g _ [] = []
    g d (x:xs) = if null x then replicate d "" ++ (x:xs) else (x : g d xs)

segsToWords :: [String] -> [Word64]
segsToWords = fmap (toEnum . foldl acc 0)

accShort :: Word64 -> Word64 -> Word64
accShort b a = b * 65536 + a

words2IPv6 :: [Word64] -> IPAddress6
words2IPv6 (a:b:c:d:xs) = IPAddress6 (foldl accShort 0 [a, b, c, d]) (foldl accShort 0 xs)
words2IPv6 _ = IPAddress6 0 0

parseIPv6 :: Parser IPAddress6
parseIPv6 = words2IPv6 . segsToWords . fillSegs <$> parseSegs

main :: IO ()
main = do
  testParse one
  print $ parseString (string "1") mempty "1"
  print $ parseString (myString "123") mempty "123"
  print $ parseString int' mempty "2341"

  -- parseRational
  print $ parseString parseRational mempty "123"
  print $ parseString parseRational mempty "123/10"

  -- parsePhone
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"

  -- parseIpv4
  print $ parseString parseIpv4 mempty "172.16.254.1"
  print $ parseString parseIpv4 mempty "204.120.0.15"

  -- parseIPv6
  print $ parseString hexadecimal mempty "xFE01"
  print $ parseString parseIPv6 mempty "0:0:0:0:0:ffff:ac10:fe01"
  print $ parseString parseIPv6 mempty "0:0:0:0:0:ffff:cc78:f"
  print $ parseString parseIPv6 mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
  print $ parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329"
  print $ parseString parseIPv6 mempty "2001:DB8::8:800:200C:417A"
