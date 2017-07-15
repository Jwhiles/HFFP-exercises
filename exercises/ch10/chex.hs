stops = "pbtdkg"
vowels = "aeiou"

f xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs]
f2 xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs, x == 'p']

-- seekritFunc returns the average length of a list of words

seekritFract :: String -> Double
seekritFract x = total / len
  where total = fromIntegral (sum (map length (words x)))
        len   = fromIntegral (length (words x))


myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x y -> x == a || y) False
myElem' :: Eq a => a -> [a] -> Bool
myElem' a = myAny (== a)

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\a b -> if pred a then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) [] 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr(\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\a b -> if f a b == GT then a else b)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\a b -> if f a b == LT then a else b)

