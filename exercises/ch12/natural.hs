data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n = go n 0
  where go Zero i     = i
        go (Succ nat) i = go nat (i + 1)

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = go i Zero
    where go 0 nat = Just nat
          go int nat = go (int - 1) (Succ nat)
