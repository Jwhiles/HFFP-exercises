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

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr
                     (\x (as, bs) -> case x of (Left a)  -> (a:as, bs)
                                               (Right b) -> (as, b:bs))
                                               ([], []) 
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f xs = either' (\_ -> Nothing) (\x -> Just (f x)) xs
