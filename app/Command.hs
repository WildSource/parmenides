{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where

import Prelude hiding ((<>))

import Text.PrettyPrint
  ( Doc
  , render
  )

startingGap :: Int
startingGap = 4

characterLimit :: Int
characterLimit = 15

dynamicGap :: Int -> String -> String
dynamicGap charLimit word =
  replicate (charLimit - (Prelude.length word)) ' '

data Command = Command
  { name :: String
  , run :: ([String] -> IO ())
  , helpDoc :: Doc
  }

instance Show Command where
  show (Command name _ helpDoc) =
    "Command name = " ++ name ++ ", helpDoc = " ++ render helpDoc

