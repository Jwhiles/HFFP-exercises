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
