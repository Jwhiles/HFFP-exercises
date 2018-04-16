{-# LANGUAGE PackageImports #-}
import Data.Traversable
import Data.Monoid

import Prelude hiding (Either, Left, Right)
import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import "checkers" Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)

-- constant
newtype Constant a b = 
  Constant { getConstant :: a } 
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = (Constant x) 

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  x <*> y = Constant (getConstant x <> getConstant y)

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = fmap Constant $ pure x

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = genConst

genConst :: Arbitrary a => Gen (Constant a b)
genConst = do
  a <- arbitrary
  return $ Constant a

-- Maybe
data Optional a =
    Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse f (Yep x) = fmap Yep $ f x
  traverse _ Nada = pure Nada

instance Eq a => EqProp (Optional a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOpt

genOpt :: Arbitrary a => Gen (Optional a)
genOpt = do
  a <- arbitrary
  frequency [ (5, return $ Yep a)
            , (1, return Nada) ]

data List a = Nil | Cons a (List a) deriving (Eq, Show, Ord)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x l) = Cons (f x) (fmap f l)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x l) = (f x) <> (foldMap f l)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x l) = fmap Cons (f x) <*> traverse f l


instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [ (3, return $ Cons h t)
            , (1, return Nil) ]

data Three a b c = Three a b c deriving (Eq, Show, Ord)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
  traverse f (Three x y z) = fmap (Three x y) $ f z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

data Three' a b = Three' a b b deriving (Eq, Show, Ord)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' _ y x) = f y <> f x

instance Traversable (Three' a) where
   traverse f (Three' a y z) = Three' a <$> f y <*> f z

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three' x y z

listTrigger = undefined :: List (Int, Int, [Int])
optionalTrigger = undefined :: Optional (Int, Int, [Int])
constantTrigger = undefined :: Constant Int (Int, Int, [Int])
threeTrigger = undefined :: Three Int Int (Int, Int, [Int])
threeTrigger' = undefined :: Three' Int (Int, Int, [Int])

main :: IO ()
main = do
  putStrLn "Constant"
  quickBatch (traversable constantTrigger)
  putStrLn "Optional"
  quickBatch (traversable optionalTrigger)
  putStrLn "List"
  quickBatch (traversable listTrigger)
  putStrLn "Three"
  quickBatch (traversable threeTrigger)
  putStrLn "Three'"
  quickBatch (traversable threeTrigger')
