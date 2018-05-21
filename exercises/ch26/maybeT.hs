module MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure . pure) x

  (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ fmap (<*>) mf <*> ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  
  (MaybeT ma) >>=  f = 
          MaybeT $ do
          v <- ma

          case v of
            (Just a) -> runMaybeT (f a)
            Nothing -> return Nothing


-- the <- operator is letting us get some value from a monad
-- imagine >>= taking a value from a monad and putting it into a function that
-- returns another monad. <- represents the first part of this

-- [5] >>= (\x -> [x])
-- (m a) >>= (\x -> pure x)
--
-- x <- (m a)
-- pure x
