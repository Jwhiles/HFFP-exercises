import Data.Monoid

mySum  :: (Foldable t, Num a) => t a -> a
mySum xs = getSum $ foldMap Sum xs

myProduct :: (Foldable t, Num a) => t a -> a
myProduct xs = getProduct $ foldMap Product xs

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem e xs = foldr (\x acc -> x == e || acc) False xs

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum xs = foldr compare Nothing xs
  where compare x Nothing = Just x
        compare x (Just y) = Just $ min x y

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum xs = foldr compare Nothing xs
  where compare x Nothing = Just x
        compare x (Just y) = Just $ max x y

myNull :: (Foldable t) => t a -> Bool
myNull xs = foldr (\x a -> False) True xs

myLength :: (Foldable t) => t a -> Int
myLength = foldr (\_ a -> a + 1) 0

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (\x acc -> x:acc) [] 

myFold :: (Foldable t, Monoid m) => t m -> m
-- myFold = foldr (\x y -> x <> y) mempty
-- myFold = foldMap id -- woah
myFold = myFoldMap id 

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f xs = foldr (\x acc -> (f x) <> acc) mempty xs


