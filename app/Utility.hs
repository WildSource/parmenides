{-|
Module: Utility
Description: Contains various self contained data procedures
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module contains various boilerplaty data fetching and/or formatting
in the form of functions to call.
-}
{-# LANGUAGE NamedFieldPuns #-}
module Utility
  ( Timestamp (year, month, day, hour, minute, second)
  , getTimestamp
  , headTail
  ) where

-- Prelude imports
import Data.Fixed (Pico)
import Data.Time
  ( getCurrentTime
  , utcToLocalTime
  , getCurrentTimeZone
  , localTimeOfDay
  , TimeOfDay (..)
  , UTCTime (..)
  )
import Data.Time.Calendar(toGregorian)
import Data.List.NonEmpty (NonEmpty(..))

headTail :: NonEmpty a -> (a, [a])
headTail (x:|xs) = (x, xs)

data Timestamp = Timestamp
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  }

instance Show Timestamp where
  show (Timestamp year month day hour minute second) =
    let fields =
          [ year
          , month
          , day
          , hour
          , minute
          , second
          ]
    in  (foldl (++) "" $ fmap pad fields)
    where
      pad :: Int -> String
      pad num
        | isDigit' num == True = "0" ++ show num
        | otherwise = show num

      isDigit' :: Int -> Bool
      isDigit' numericValue
        =  numericValue >= 0
        && numericValue <= 9

getTimestamp :: IO Timestamp
getTimestamp = do
  time <- getCurrentTime
  timezone <- getCurrentTimeZone
  let (year, month, day) = toGregorian $ utctDay time
  let localTime = localTimeOfDay $ utcToLocalTime timezone time
  let hour = todHour localTime
  let minutes = todMin localTime
  let second = takeSeconds $ todSec localTime
  
  pure $ Timestamp (fromIntegral year) month day hour minutes second
  where
    takeSeconds :: Pico -> Int
    takeSeconds = (read . takeWhile (\ c -> c /= '.') . show)
  
