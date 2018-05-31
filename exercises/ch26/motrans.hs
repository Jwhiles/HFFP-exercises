module Mt where

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (EitherT e) where 
  lif
