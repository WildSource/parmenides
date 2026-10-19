{-# LANGUAGE NamedFieldPuns #-}
module Utility where

import Data.Time
import Data.Time.Calendar(toGregorian)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text qualified as T

headTail :: NonEmpty a -> (a, [a])
headTail (x:|xs) = (x, xs)

data Timestamp = Timestamp
  { year :: Year
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  }

instance Show Timestamp where
  show (Timestamp year month day hour minute) =
    let fields =
          [ fromIntegral year
          , month
          , day
          , hour
          , minute
          ]
    in  foldl (++) "" $ fmap show fields


getTimestamp :: IO Timestamp
getTimestamp = do
  time <- getCurrentTime
  timezone <- getCurrentTimeZone
  let (year, month, day) = toGregorian $ utctDay time
  let localTime = localTimeOfDay $ utcToLocalTime timezone time
  let hour = todHour localTime
  let minutes = todMin localTime
  
  pure $ Timestamp year month day hour minutes
  
