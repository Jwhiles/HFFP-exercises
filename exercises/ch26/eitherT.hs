module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ pure . pure $ x

  (EitherT f) <*> (EitherT ema) = EitherT $ fmap (<*>) f <*> ema

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (EitherT ema) >>= f = EitherT $ do
                 v <- ema
                 case v of (Left e) -> return (Left e)
                           (Right a) -> runEitherT (f a)

swapEither :: Either a b -> Either b a
swapEither (Left x) = (Right x)
swapEither (Right x) = (Left x)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT e = EitherT $ swapEither <$> runEitherT e

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT amb) = amb >>= (\v ->
                                      case v of (Left e) -> f e
                                                (Right a) -> g a)
-- eitherT f g (EitherT amb) = amb >>= h
--                    where h (Left e) = f e
--                          h (Right a) = g a
