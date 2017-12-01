import Data.Monoid
data Validation e a =
  Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

instance Monoid e =>
         Applicative (Validation e) where
  pure x = Success x
  (Failure a) <*> (Failure b) = Failure (a <> b)
  (Success f) <*> (Failure b) = Failure (b)
  (Failure a) <*> (Success b) = Failure (a)
  (Success f) <*> (Success x) = Success (f x)
