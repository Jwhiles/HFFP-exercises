data OperatingSystem =
    GnuPlusLinux
  | OpenBSDP
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Brook
  | Curry
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDP
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Brook, Curry]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer { os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]

