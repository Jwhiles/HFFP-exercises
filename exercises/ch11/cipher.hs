import Data.Char

type Message = String
type Seed = String

alphabetPosition :: Char -> Int
alphabetPosition c
  | isUpper c = ord c - 65
  | isLower c = ord c - 97
  | otherwise = ord c

cipher :: Int -> Char -> Char
cipher n c
  | isUpper c = chr $ (mod (alphabetPosition c + n) 26) + 65
  | isLower c = chr $ (mod (alphabetPosition c + n) 26) + 97
  | otherwise = c

unCipher :: Int -> Char -> Char
unCipher n c = cipher (-n) c

vCipher :: Seed -> Message -> Message
vCipher s m = zipWith cipher shifts m
  where shifts = map alphabetPosition (take (length m) (cycle s))

unVChipher :: Seed -> Message -> Message
unVChipher s m = zipWith unCipher shifts m
  where shifts = map alphabetPosition (take (length m) (cycle s))
