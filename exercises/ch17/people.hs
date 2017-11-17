validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
    then Nothing
    else Just s

validateHeight :: Int -> Int -> Maybe Int
validateHeight max h = if h > max then Nothing else Just h

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
newtype Height = Height Int deriving (Eq, Show)


mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = fmap Address $ validateLength 100 s

mkHeight :: Int -> Maybe Height
mkHeight cm = fmap Height $ validateHeight 250 cm

data Person = Person Name Address Height deriving (Eq, Show)

mkPerson :: String -> String -> Int -> Maybe Person
mkPerson n a h = Person <$> 
                   mkName n <*> 
                   mkAddress a <*> 
                   mkHeight h

