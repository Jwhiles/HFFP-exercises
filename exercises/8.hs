-- recursion 1
sumToN :: Integer -> Integer
sumToN num = go num 0 0
 where go n i c 
        | i > n     = (c)
        | otherwise = go n (i + 1) (c + i)

intMult :: (Integral a) => a -> a -> a
intMult x y = go x (y - 1) x 
 where go a b prod
        | b == 0 = prod
        | otherwise = go a (b - 1) (prod + a)
