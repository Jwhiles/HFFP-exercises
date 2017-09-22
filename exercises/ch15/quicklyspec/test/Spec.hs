import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = (mempty <> a) == a

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = (a <> mempty) == a

type S = String
type B = Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: S -> S -> S -> B)
--   quickCheck (monoidLeftId :: S -> B)
--   quickCheck (monoidRightId :: S -> B)

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools -- will fail because we never maintain a twoo

type BullMappend = Bull -> Bull -> Bull -> Bool

bullish :: IO ()
bullish = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftId :: Bull -> Bool)
  quickCheck (monoidRightId :: Bull -> Bool)

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = (Only (a <> b))
  mappend (Only a) _ = (Only a)
  mappend _ (Only a) = (Only a)
  mappend Nada Nada = Nada

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = undefined
  mappend = undefined

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend
type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

fster :: IO ()
fster = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftId :: FstId)
  quickCheck (monoidRightId :: FstId)
