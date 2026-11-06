module Log where

import System.Exit (die)

import System.Console.ANSI
import System.Console.ANSI.Types (Color (..))
import Text.PrettyPrint
  ( nest
  , render
  , text
  , (<+>)
  , Doc
  )

reportError :: String -> IO ()
reportError message
  =  setSGR [SetColor Foreground Dull Red]
  >> die ("Error: " ++ (render $ text message))

