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
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn "How old are you?"
  age <- getLine
  case mkPerson name (read age :: Age) of
    (Left a) -> putStrLn ("there was an error in your person: " ++ show a)
    (Right a) ->  putStrLn ("Yay! a real person: " ++ show a)
