lefts' :: [Either a b] -> [a]
lefts' = foldr 
          (\x acc -> case x of (Left a)  -> a:acc
                               (Right b) -> acc)
          []

rights' :: [Either a b] -> [b]
rights' = foldr 
          (\x acc -> case x of (Left a)  -> acc
                               (Right b) -> b:acc)
          []


