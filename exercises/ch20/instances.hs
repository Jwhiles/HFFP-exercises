{-# LANGUAGE InstanceSigs #-}
import Data.Monoid

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap _ _ = mempty


data Two a b = Two a b

instance Foldable (Two a) where
  foldMap :: Monoid m => (b -> m) -> (Two a b) -> m
  foldMap f (Two _ x) = f x

  foldr :: (x -> b -> b) -> b -> (Two a x) -> b
  foldr f acc (Two _ x) = f x acc

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap :: Monoid m => (c -> m) -> (Three a b c) -> m
  foldMap f (Three _ _ x) = f x

data Three' a b = Three' a b b 

instance Foldable (Three' a) where
  foldMap :: Monoid m => (b -> m) -> (Three' a b) -> m
  foldMap f (Three' _ x y) = f x <> f y

data Four' a b = 
  Four' a b b b

instance Foldable (Four' a) where
  foldMap :: Monoid m => (b -> m) -> (Four' a b) -> m
  foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF predicate xs = foldMap mapper xs
  where mapper x = if predicate x then pure x else mempty

