myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- need to also deal with the maybeness
myUnfoldr f x = (fst $ f x):(myUnfoldr f (snd $ f x))
