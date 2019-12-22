import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = intercalate " " . map g . words
  where
    g :: String -> String
    g "the" = "a"
    g s = s

vowel :: Char -> Bool
vowel ch = elem ch "aeiou"

startWithVowel :: String -> Bool
startWithVowel (ch : _) = vowel ch
startWithVowel _ = False

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s =
  fromIntegral . length . filter f $ zip w (tail w)
  where
    w = words s
    f :: (String, String) -> Bool
    f ("the", b) = startWithVowel b
    f _ = False

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter vowel
