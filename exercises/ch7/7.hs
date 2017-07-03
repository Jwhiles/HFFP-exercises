-- Grab bag
-- 1
-- the four functions are all equivalent

-- 2
-- mth3 has the type signature Num a => a -> a -> a -> a

-- 3

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip x y f = f y x

-- Guard duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'a'
  | y >= 0.8 = 'b'
  | y >= 0.7 = 'c'
  | y >= 0.6 = 'd'
  | y <  0.6 = 'f'
  where y = x / 100
-- guard blocks evaluate top to bottom, returning
-- as soon as a condition evaluates to true

-- pal returns True for palindromes
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

-- numbers returns an indication of whether 
-- the argument is positive or negative
numbers :: (Ord a, Num a) => a -> a
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
