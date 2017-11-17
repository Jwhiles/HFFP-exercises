import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap (g . f) x == (fmap g (fmap f x))

newtype Identity a = Identity a
  deriving (Eq)

instance Functor Identity where
  fmap f (Identity i) = Identity (f i)


data Pair a = Pair a a
  deriving (Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)


data Two a b = Two a b
  deriving (Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c
  deriving (Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

data Four a b c d = Four a b c d
  deriving (Eq)

instance Functor (Four a b c) where
  fmap f (Four x y z p) = Four x y z (f p)

data Four' a b = Four' a a a b
  deriving (Eq)

instance Functor (Four' a) where
  fmap f (Four' x y z p) = Four' x y z (f p)

data Trivial = Trivial
-- There is no valid functor instance for Trivial. It has the kind of * whilst Functors must have the type * -> * 
-- <3

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (Identity (x :: Int))
  quickCheck $ \x y -> functorIdentity (Pair (x :: Int) (y :: Int))
  quickCheck $ \x y -> functorIdentity (Two (x :: String) (y :: Int))
  quickCheck $ \x y z -> functorIdentity (Three (x :: String) (y :: Int) (z :: Bool))
  quickCheck $ \x y z p -> functorIdentity (Four (x :: String) (y :: Int) (z :: Bool) (p :: Float))
  quickCheck $ \x y z p -> functorIdentity (Four (x :: String) (y :: String) (z :: String) (p :: Float))