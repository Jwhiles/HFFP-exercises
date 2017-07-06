import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
   (secondsToDiffTime 34123))
   , DbNumber 9001
   , DbString "Hello World"
   , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
   , DbNumber 2001
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (go) []
      where go (DbDate date) b = date:b
            go  _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (go) []
      where go (DbNumber i) b = i:b
            go  _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (/) (fromIntegral (sumDb db)) (fromIntegral (length (filterDbNumber db)))
