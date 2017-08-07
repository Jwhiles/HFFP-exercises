import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- non recursive solution
notThe :: String -> Maybe String
notThe "The" = Just "a"
notThe "the" = Just "a"
notThe _     = Nothing

help :: (Maybe String, String) -> String
help (Just s, _) = s
help (_, s)      = s

replaceThe :: String -> String
replaceThe s = intercalate " " $ map help perhaps
  where perhaps = zip (map notThe $ words s) (words s)

-- how to do it recursively.. I assume it needs a go function

replaceThe' :: String -> String
replaceThe' s = intercalate " " $ go (words s)
  where go []     = [] 
        go (x:xs) = (fromMaybe x (notThe x)):go xs 

-- fromMaybe could also be used in zipWith!

replaceThe'' :: String -> String
replaceThe'' s = intercalate " " $ zipWith fromMaybe w t
  where w = words s
        t = map notThe w

-- is there a cleaner way to do this - without intercalate? idk
--

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where 
    go [] c       = c 
    go (x:[]) c   = c
    go (x:y:xs) c = if x == "the" && elem (head y) "aeiou" 
                        then go (y:xs) (c + 1)
                        else go (y:xs) c

countVowels :: String -> Int
countVowels = length . filter (\l -> elem l "aeiou")
