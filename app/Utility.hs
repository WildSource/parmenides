module Utility where

import Data.Time
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.List.NonEmpty (NonEmpty(..))

headTail :: NonEmpty a -> (a, [a])
headTail (x:|xs) = (x, xs)

data Timestamp = Timestamp
  { year :: Year
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  } deriving Show

getTimestamp :: IO Timestamp
getTimestamp = do
  time <- getCurrentTime
  year <- getYear time
  pure $ Timestamp year undefined undefined undefined undefined
  where
    getYear :: UTCTime -> IO Year
    getYear utct = do
        let day = utctDay utct
        let (year, _) = toOrdinalDate day
        pure year

