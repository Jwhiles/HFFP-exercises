myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem a (x:xs) = x == a || myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = myAny (\x -> x == a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish xs = concat xs

squish' :: [[a]] -> [a]
squish' [] = []
squish' (x:xs) = x ++ squish' xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ []         = Nothing
myMaximumBy _ (x:[])     = Just x
myMaximumBy f (x:(y:xs)) = if f x y == GT
                             then myMaximumBy f (x:xs)
                             else myMaximumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum = myMaximumBy (compare)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy _ []         = Nothing
myMinimumBy _ (x:[])     = Just x
myMinimumBy f (x:(y:xs)) = if f x y == LT
                             then myMinimumBy f (x:xs)
                             else myMinimumBy f (y:xs)

myMinimum :: (Ord a) => [a] -> Maybe a
myMinimum =  myMinimumBy (compare)
