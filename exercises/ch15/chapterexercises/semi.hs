import Test.QuickCheck
import Data.Semigroup

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving(Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
   x <- arbitrary
   return (Identity x)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x y z p) <> (Four x' y' z' p') = Four (x <> x') (y <> y') (z <> z') (p <> p')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  p <- arbitrary
  return (Four x y z p)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- 6
data BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String [Int])
  quickCheck (semigroupAssoc :: ThreeAssoc String [Int] String)
  quickCheck (semigroupAssoc :: FourAssoc String [Int] String [Int])
  quickCheck (semigroupAssoc :: BoolConjAssoc)
