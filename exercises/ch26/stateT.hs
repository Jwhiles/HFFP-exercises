module StateT where
import Control.Lens
import Control.Lens.Tuple

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- fmap f (StateT smas) = StateT $ \s -> (\(a, s') -> (f a, s')) <$> smas s
  fmap f (StateT smas) = StateT $ \s -> (_1 %~ f) <$> smas s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> (pure (a, s))
  (<*>) (StateT smf) (StateT sma) = StateT $ \s -> do
                          (f, s') <- smf s
                          (_1 %~ f) <$> sma s'

  

instance Monad m => Monad (StateT s m) where
  return = pure

  
  StateT smas >>= f = StateT $ \s -> do
                (a, s') <- smas s
                runStateT (f a) s'


                
