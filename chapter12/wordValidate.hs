newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel ch = elem ch vowels

validWord :: String -> Bool
validWord w =
  numOfVowels <= numOfConsonants
  where
    numOfVowels = length . filter isVowel $ w
    numOfConsonants = length . filter (not . isVowel) $ w

mkWord :: String -> Maybe Word'
mkWord s = if validWord s then Just (Word' s) else Nothing 
