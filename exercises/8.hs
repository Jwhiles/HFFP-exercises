-- recursion 1
-- sumToN :: Integer -> Integer
-- sumToN num = go num 0 0
--  where go n i c 
--         | i > n     = (c)
--         | otherwise = go n (i + 1) (c + i)
-- 
-- intMult :: (Integral a) => a -> a -> a
-- intMult x y = go x (y - 1) x 
--  where go a b prod
--         | b == 0 = prod
--         | otherwise = go a (b - 1) (prod + a)
-- 
-- mc91 :: (Num a, Ord a) => a -> a
-- mc91 n
--  | n > 100   = n - 10
--  | otherwise = mc91 . mc91 $ n + 11
-- 
-- numbers into words
module WordNumber where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "more than one digit"

digits :: Int -> [Int]
digits i = go i
  where go d
         | d < 1 = []
         | otherwise = go (d `div` 10) ++ [d `mod` 10] 

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord $ digits n)
