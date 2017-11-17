import           Control.Applicative
import           Data.List           (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

tupled' :: Maybe (Integer, Integer)
tupled' = liftA2 (,) y z


x :: Maybe Int
x = elemIndex 3 [1..5]

p :: Maybe Int
p = elemIndex 4 [1..5] -- elem index returns the first index of an item

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> p

xs = [1..3]
ys = [4..6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (pure sum) <*> ((,) <$> x' <*> y')
