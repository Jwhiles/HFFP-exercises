-- type Name = String
-- type Age = Integer

-- data Person = Person Name Age deriving Show

-- data PersonInvalid = NameEmpty
--                    | AgeTooLow
--                    deriving (Eq, Show)

-- mkPerson :: Name -> Age -> Either PersonInvalid Person
-- mkPerson n a
--   | n /= "" && a >= 0 = Right $ Person n a
--   | n == "" = Left NameEmpty
--   | otherwise = Left AgeTooLow
--
type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow deriving(Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
  | name /= "" = Right name
  | otherwise = Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person

mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left (badName)
mkPerson' _              (Left badAge) = Left (badAge)


myFunc :: Either a b -> Bool
myFunc (Left _) = True
myFunc (Right _) = False


