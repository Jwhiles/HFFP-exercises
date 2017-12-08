import Control.Monad (join)
-- fmap (a -> b) -> m a -> m b
-- join Monad m => m (m a) -> m a
bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join (fmap f xs) 
