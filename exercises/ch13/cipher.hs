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

runCipher :: IO String
runCipher = do
  putStrLn "Please a seed for your cipher"
  seed <- getLine
  putStrLn "Please enter the message you would like encrypted"
  message <- getLine
  return (vCipher seed message)

runUnCipher :: IO String
runUnCipher = do
  putStrLn "Please enter the seed your message is encrypted with"
  seed <- getLine
  putStrLn "Please enter the encrypted message"
  message <- getLine
  return (unVChipher seed message)
