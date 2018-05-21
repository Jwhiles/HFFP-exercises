{-# LANGUAGE InstanceSigs  #-}
module ID where

import Control.Monad


newtype IdentityT f a = IdentityT { runIdentityt :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ (fmap f ma)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x

  (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: (IdentityT m a) -> (a -> IdentityT m b) -> (IdentityT m b)
  (IdentityT ma) >>= f = IdentityT $ ma >>= (runIdentityt . f)
