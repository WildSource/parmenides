{-|
Module: Log
Description: Logging module
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module contains functions to log various events.
-}
module Log (reportError) where 

-- Prelude imports
import System.Exit (die)

-- Library imports
import System.Console.ANSI
import Text.PrettyPrint
  ( render
  , text
  )

-- TODO fix bug where red color applies after the log
reportError :: String -> IO ()
reportError message
  =  setSGR [SetColor Foreground Dull Red]
  >> die ("Error: " ++ (render $ text message))
  >> setSGR [Reset]

