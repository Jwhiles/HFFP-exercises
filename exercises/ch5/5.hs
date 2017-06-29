-- Fix it

-- 1 
-- module Sing where -- fix capitalisation
-- 
-- fstString :: [Char] -> [Char] -- type signatures should use -> rather than specify operations
-- fstString x = x ++ " in the rain"
-- 
-- sndString :: [Char] -> [Char] -- returns a string not char
-- sndString x = x ++ " over the rainbow"
-- 
-- sing = if (x > y) then fstString x else sndString y -- else rather than or
--   where x = "Singin"
--         y = "Somewhere"

-- 3 
-- module Arith3Broken where
-- 
-- main :: IO ()
-- main = do -- main shouldn't be capitalised
--   print $ 1 + 2 -- ensure we evaluate 1 + 2 before printing
--   putStrLn $ show 10 -- convert Num to string to use putStr
--   print (negate (-1)) -- wrap negative numbers in parens 
--   print ((+) 0 blah)
--   where blah = negate 1

-- Type-Kwon-Do

-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g $ f i

-- 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x 

-- 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x






