import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord []    = []
capitalizeWord (x:xs)= toUpper x:xs


