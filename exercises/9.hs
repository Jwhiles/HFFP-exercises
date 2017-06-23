-- Enum doesn't imply an instance of Ord 
-- but we can use fromEnum to force the same behaviour
-- ??

eftOrd :: (Enum a, Ord a) => a -> a -> [a]
eft from to = go from to [] 
  where go f t result
         | f > t = reverse result
         | otherwise = go (succ f) t (f:result)

eft :: (Enum a) => a -> a -> [a]
eft from to = go from to [] 
  where go f t result
         | fromEnum f > fromEnum t = reverse result
         | otherwise = go (succ f) t (f:result)
         
