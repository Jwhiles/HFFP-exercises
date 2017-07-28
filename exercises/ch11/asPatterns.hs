fibo :: [Integer]
fibo = go [2,1]
  -- where go xs@(x:y:_) = go ((x+y):xs)
  where go (x:y:xs) = go ((x+y):x:y:xs)
