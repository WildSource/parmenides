{-# LANGUAGE NamedFieldPuns #-}
module Utility where

import Data.Fixed (Pico)
import Data.Time
  ( getCurrentTime
  , utcToLocalTime
  , getCurrentTimeZone
  , localTimeOfDay
  , TimeOfDay (..)
  , Year
  , UTCTime (..)
  )
import Data.Time.Calendar(toGregorian)
import Data.List.NonEmpty (NonEmpty(..))

headTail :: NonEmpty a -> (a, [a])
headTail (x:|xs) = (x, xs)

data Timestamp = Timestamp
  { year :: Year
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  }

instance Show Timestamp where
  show (Timestamp year month day hour minute second) =
    let fields =
          [ fromIntegral year
          , month
          , day
          , hour
          , minute
          , second
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
  let second = takeSeconds $ todSec localTime
  
  pure $ Timestamp year month day hour minutes second
  where
    takeSeconds :: Pico -> Int
    takeSeconds = (read . takeWhile (\ c -> c /= '.') . show)
  
