data List a = 
  Nil | Cons a (List a) deriving (Eq, Show)

-- take' :: Int -> List a -> List a
-- take' = 

instance Functor List where
  fmap f (Cons x (xs)) = Cons (f x) (fmap xs)
  fmap _ Nil = Nil

-- instance Applicative List where
--   pure x = Cons x (Nil)
  -- (<*>) a b = undefined

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--   xs =-= ys = xs' `eq` ys'
--     where xs' = let (ZipList' l) = xs
--                 in take' 3000 l
--           ys' = let (ZipList' l) = ys
--                 in take' 3000 l

-- instance Functor ZipList' where
--   fmap f (ZipList' xs) = ZipList' $ fmap fs xs
