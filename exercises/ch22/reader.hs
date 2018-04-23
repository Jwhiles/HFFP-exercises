{-# LANGUAGE InstanceSigs #-}
newtype Reader r a = 
  Reader { runReader :: r -> a }


instance Functor (Reader r) where
  fmap :: (a -> b)
       -> Reader r a
       -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra) 

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a 
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb
