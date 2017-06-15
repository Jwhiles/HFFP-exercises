-- 1
-- D

-- 2
f :: Char -> String
f = undefined
g :: String -> [String]
g = undefined

h = g . f
-- has type signature of Char -> [String]
-- 3
three :: Ord a => a -> a -> Bool
three = undefined
-- applying Num a to three gives the type Num a -> Bool

-- 4
-- a function with the type (a -> b) -> c is a higher order function

-- 5 Applying a value to id returns the value. so f True :: Bool

-- Let's write code
-- 1
tensDigit :: Integral a => a -> a
tensDigit x = d 
  where (xLast, _) = divMod x 10
        (_, d)     = divMod xLast 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d2
  where (xLast, _) = divMod x 100
        (_, d2)     = divMod xLast 10

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
  True  -> x
  False -> y
foldBool' :: a -> a -> Bool -> a
foldBool' x y z
  | z          = x
  | otherwise  = y

-- 3
higher :: (a -> b) -> (a, c) -> (b, c)
higher f (x, y) = (f x, y)

-- 4








