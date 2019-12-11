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
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr g []
  where
    g :: DatabaseItem -> [UTCTime] -> [UTCTime]
    g (DbDate utcTime) b = utcTime : b
    g _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr g []
  where
    g :: DatabaseItem -> [Integer] -> [Integer]
    g (DbNumber a) b = a : b
    g _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = let p = foldr (\x (s, count) -> (s + x, count + 1)) (0, 0) . filterDbNumber $ db in (fromInteger . fst $ p) / (fromInteger . snd $ p)
