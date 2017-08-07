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
