{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where

import Data.Char (toUpper)

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint
  ( nest
  , render
  , text
  , (<+>)
  , Doc
  )

startingGap :: Int
startingGap = 2

characterLimit :: Int
characterLimit = 20

dynamicGap :: Int -> String -> String
dynamicGap charLimit word =
  replicate (charLimit - (Prelude.length word)) ' '

mkHelpDoc :: String -> [String] -> String -> Doc
mkHelpDoc name [] msg =
  let name' = nest startingGap (text name)
      gap = dynamicGap characterLimit name
  in  name' PP.<> (text (gap ++ msg))
mkHelpDoc name args msg =
  let name' = nest startingGap $ text name
      gap = dynamicGap characterLimit (name ++ " " ++ show args')
      args' = text $ renderArgs args
  in  name' <+> args' PP.<> (text (gap ++ msg))
  where
    renderArgs :: [String] -> String
    renderArgs [] = []
    renderArgs (x : xs) =
      let arg = fmap toUpper x
      in  arg ++ " " ++ renderArgs xs

data Command = Command
  { name :: String
  , run :: ([String] -> IO ())
  , helpDoc :: Doc
  }

instance Show Command where
  show (Command name _ helpDoc) =
    "Command name = " ++ name ++ ", helpDoc = " ++ render helpDoc

