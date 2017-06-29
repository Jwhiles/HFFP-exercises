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

myWords3 :: String -> [String]
myWords3 [] = []
myWords3 (' ':rest) = myWords3 rest
myWords3 s = w: myWords3 t
  where
    w = takeWhile(/= ' ') s
    t = dropWhile(/= ' ') s

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = [] 
myLines ('\n':s) = myLines s
myLines s = w: myLines t
  where
    w = takeWhile(/= '\n') s
    t = dropWhile(/= '\n') s

splitAtChar :: Char -> String -> [String]
splitAtChar c s = go s
  where
    go str
     | str == [] = []
     | head str == c = go (tail str)
     | otherwise = w : go t
                  where 
                    w = takeWhile(/= c) str
                    t = dropWhile(/= c) str

