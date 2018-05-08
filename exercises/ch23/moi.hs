{-# LANGUAGE InstanceSigs #-}
module Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \st -> let (a, s) = g st in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)


  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \st -> let (a, s) = g st
                                         (aTOb, s') = f s
                                     in (aTOb a, s')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g =
    Moi $ \st -> let (a, s) = f st
                  in runMoi (g a) s


-- -- f :: s -> (a -> b, s)
-- -- g :: s -> (a, s)
