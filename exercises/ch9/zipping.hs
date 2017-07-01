jZip :: [a] -> [b] -> [(a, b)]
jZip [] _          = []
jZip _ []          = []
jZip (x:xs) (y:ys) = (x, y) : jZip xs ys

jZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
jZipWith _ [] _          = []
jZipWith _ _ []          = []
jZipWith f (x:xs) (y:ys) = f x y : jZipWith f xs ys

jZip' :: [a] -> [b] -> [(a, b)]
jZip' = jZipWith (\x -> \y -> (x, y))
