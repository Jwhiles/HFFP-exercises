import GHC.Arr

data Bool =
  False | True 
  -- no functor is possible

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a =
  Falish | Truish a
  deriving (Eq)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falish = Falish
  fmap f (Truish a) = Truish (f a)

-- newtype Mu f = InF { outF :: f (Mu f) }
--   deriving (Eq)

-- instance Functor (Mu g) where
--   fmap f (InF x) = InF (f x)

-- data D = 
--   D (Array Word Word) Int Int

-- instance Functor D (arr w p) i where


-- rearrange

data Sum b a =
  First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
  DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = 
  L a b a | R b a b

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant d) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

data K a b =
  K a

instance Functor (K e) where
  fmap f (K x) = K x

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor h, Functor g) => Functor (Parappa h g) where
  fmap f (DaWrappa ha ga) = DaWrappa (fmap f ha) (fmap f ga) -- all intances of a have to be fmapped to ensure they stay the same type :)


data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap fu (IgnoringSomething fa gb) = IgnoringSomething fa (fmap fu gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons start rest) = Cons (f start) (fmap f rest)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where 
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl gl' gl'') = MoreGoats (fmap f gl) (fmap f gl') (fmap f gl'')

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read (f . sa)