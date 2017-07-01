import Data.Char

filterLower :: String -> String
filterLower = filter (isUpper)

capFirst :: String -> String
capFirst []     = []
capFirst (x:xs) = toUpper x : xs

capAll :: String -> String
capAll []     = []
capAll (x:xs) = toUpper x : capAll xs

firstToUpper :: String -> Char
firstToUpper = head . capFirst
