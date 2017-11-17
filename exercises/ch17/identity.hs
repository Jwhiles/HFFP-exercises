import Data.Monoid
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)


newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (<*>) a a' = Constant ( getConstant a <> getConstant a')
