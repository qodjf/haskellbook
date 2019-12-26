type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please input your name:"
  name <- getLine
  putStrLn "Please input your age:"
  ageLine <- getLine
  (let
      age = read ageLine
      maybePerson = mkPerson name age
   in case maybePerson of
     (Right person) ->
        putStrLn ("Yay! Successfully got a person: " ++ (show person))
     (Left error) ->
        putStrLn ("You got an error: " ++ (show error)))
