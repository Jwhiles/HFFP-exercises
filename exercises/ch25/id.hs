{-# LANGUAGE InstanceSigs #-}
module Id where
import Data.Foldable

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure $ pure x

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a
  --             or           = Compose $ ((<*>) <$> f <*> a)

  -- we fmap <*> through the first layer of applicatives. So we have a something
  -- like [(Just f <*>), Nothing, (Just f <*>)] then we apply that list of
  -- functions to [(Just 5), Nothing] or whatever

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap am (Compose fg) = foldMap (foldMap am) fg

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fgx) = fmap Compose ((traverse . traverse) f fgx)
  
