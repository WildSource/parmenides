{-|
Module: Log
Description: Logging module
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module contains functions to log various events.
-}
module Log
  ( reportError
  , reportErrorWithException
  ) where 

-- Prelude imports
import System.Exit (die)
import Control.Exception
  ( Exception
  , displayException
  )

-- Library imports
import System.Console.ANSI
import Text.PrettyPrint
  ( render
  , text
  )

reportError :: String -> IO ()
reportError message
  =  setSGR [SetColor Foreground Dull Red]
  >> putStr "Error: "
  >> setSGR [Reset]
  >> putStr (render $ text (message ++ "\n"))
  >> die ""

reportErrorWithException :: Exception e => String -> e -> IO ()  
reportErrorWithException message exception = do
  setSGR [SetColor Foreground Dull Red]
  putStr "Error: "
  setSGR [Reset]
  putStr (render $ text (message ++ "\n"))
  putStrLn (displayException exception)
  die ""
  
