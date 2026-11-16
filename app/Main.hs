{-|
Module: Main
Description: The entry point for the program
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module parses the inputs from IO and executes a command from them.
-}
module Main (main) where

-- Prelude imports
import System.Environment (getArgs)

-- Library imports
import LoadEnv (loadEnvFrom)

-- User code imports
import Log (reportError)
import Commands (getCmd)
import Command
  ( Command (..)
  , executeCommand
  )

main :: IO ()
main = do
  loadEnvFrom "parmenides.env"
  args <- getArgs
  let parsedCmd = parseCmd args
  case parsedCmd of
    Left err -> err
    Right (cmd, args') -> executeCommand cmd args'

parseCmd :: [String] -> Either (IO ()) (Command, [String])
parseCmd [] = Left $ reportError "No command provided"
parseCmd (x:xs) =
  case getCmd x of
    Nothing -> Left $ reportError "Faulty command name"
    Just cmd -> Right (cmd, xs)

