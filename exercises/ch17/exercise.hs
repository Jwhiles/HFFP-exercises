import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f x = pure f <*> x

instance Applicative Pair where
  pure x = (Pair x x)
  (Pair f h) <*> (Pair x y) = Pair (f x) (h y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do 
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Monoid b => Functor (Two b) where
  fmap f x = pure f <*> x

instance Monoid b => Applicative (Two b) where
  pure x = Two mempty x
  Two u f <*> Two u' x = Two (u <> u') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b) => Functor (Three a b) where
  fmap f x = pure f <*> x

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c)
         where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq



data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f h <*> Three' a' x y = Three' (a <> a') (f x) (h y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)



main :: IO ()
main = do 
  quickBatch (applicative (undefined :: Pair (Int, Int, String))) 
  quickBatch (applicative (undefined :: Two (String, String, String) (Int, Int, String))) 
  quickBatch (applicative (undefined :: Three
                                          (String, String, String)
                                          (String, String, String)
                                          (Int, Int, String))) 
  quickBatch (applicative (undefined :: Three' (String, String, String) (Int, Int, String))) 
  print $ combos stops vowels "hello"
