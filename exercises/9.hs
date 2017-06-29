<<<<<<< HEAD
safeTail :: [a] -> Maybe [a]
safeTail []      = Nothing
safeTail (x:[])  = Nothing
safeTail (_:xs)  = Just xs
=======
-- Enum doesn't imply an instance of Ord 
-- but we can use fromEnum to force the same behaviour
-- ??

eftOrd :: (Enum a, Ord a) => a -> a -> [a]
eftOrd from to = go from to [] 
  where go f t result
         | f > t = reverse result
         | otherwise = go (succ f) t (f:result)

eft :: (Enum a) => a -> a -> [a]
eft from to = go from to [] 
  where go f t result
         | fromEnum f > fromEnum t = reverse result
         | otherwise = go (succ f) t (f:result)
         

-- thy fearful symmetry
getWord :: String -> String
getWord = takeWhile (/= ' ')

removeWord :: Int -> String -> String
removeWord len words = drop len words

myWords :: String -> [String]
myWords words = go words []
  where go w acc
         | w == [] = reverse acc
         | otherwise = go newWords ((getWord w):acc)
            where newWords = removeWord ((length $ getWord w) + 1) w 

myWords2 :: String -> [String]
myWords2 words = go words []
  where go w acc
         | w == [] = reverse acc
         | otherwise = go newWords ((currentWord):acc)
            where
              currentWord = getWord w;
              newWords = removeWord ((length currentWord) + 1) w 
>>>>>>> bc13122cc222539795a972c559c739910577b38e
