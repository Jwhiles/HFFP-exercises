import Data.Monoid
 
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) _ = (Only a)
  mappend _ (Only a) = (Only a)
  mappend Nada Nada = Nada


