import Data.List (words)
import Data.Char (toUpper)

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x:xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False 
isSubsequenceOf s@(a:as) (b:bs) =
  (a == b && isSubsequenceOf as bs || isSubsequenceOf s bs)

capitaliseWords :: String -> [(String, String)]
capitaliseWords = map (capWord) . words 

capWord :: String -> (String, String)
capWord [] = ([], [])
capWord s@(x:xs) = (s, toUpper x:xs)
