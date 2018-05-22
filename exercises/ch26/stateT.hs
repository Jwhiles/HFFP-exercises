module StateT where
import Control.Lens
import Control.Lens.Tuple

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- fmap f (StateT smas) = StateT $ \s -> (\(a, s') -> (f a, s')) <$> smas s
  fmap f (StateT smas) = StateT $ \s -> (over _1 f) <$> smas s

instance Applicative m => Applicative (StateT s m) where
  pure a = StateT $ \s -> (pure (a, s))

  -- ReaderT fmab <*> ReaderT rma = ReaderT $ fmap (<*>) fmab <*> rma
  -- StateT smf <*> StateT sma = StateT $ \s -> 
  
