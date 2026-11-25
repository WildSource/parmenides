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
  , Year
  , UTCTime (..)
  )
import Data.Time.Calendar(toGregorian)
import Data.List.NonEmpty (NonEmpty(..))

-- Library imports
import Text.Regex
  ( matchRegex
  , mkRegex
  )

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

timestampFromString :: String -> Timestamp
timestampFromString str =
  let regex = mkRegex "^([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})$"
      Just [year, month, day, hour, minute, second] = matchRegex regex str
  in  Timestamp year month day hour minute second

instance Show Timestamp where
  show (Timestamp year month day hour minute second) =
    let fields =
          [ fromIntegral year
          , month
          , day
          , hour
          , minute
          ]
    in  (foldl (++) "" $ fmap show fields) ++ pad second
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
  
  pure $ Timestamp year month day hour minutes second
  where
    takeSeconds :: Pico -> Int
    takeSeconds = (read . takeWhile (\ c -> c /= '.') . show)
  
