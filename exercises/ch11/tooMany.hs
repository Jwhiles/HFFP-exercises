{-# LANGUAGE FlexibleInstances #-}
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 10

instance TooMany (Int, String) where
  tooMany (n, _) = n > 20

instance TooMany (Int, Int) where
  tooMany (a, b) = a + b > 20

instance TooMany (Num a) => (a, a) where
  tooMany (x, y) = tooMany x || tooMany y
