module Main (main) where

import Prelude hiding (error)
import Data.List.NonEmpty as NE
import Data.List.NonEmpty (nonEmpty)

import System.Environment (getArgs, getEnv)
import LoadEnv (loadEnvFrom)

import Command
  ( help
  , setup
  , setupTracking
  , mkMigration
  , error
  )

main :: IO ()
main = do
  loadEnvFrom "parmenides.env"
  args <- getArgs
  execute args
  
execute :: [String] -> IO ()
execute (x:xs) =
  case x of
    "help" -> help
    
    "setup" -> setup

    "setupTracking" -> setupTracking
    
    "mkMigration" ->
      case nonEmpty xs of
        Nothing ->
          error "mkMigration command is missing an argument representing the migration's title"
        Just nonEmptyArgs ->
          mkMigration $ NE.head nonEmptyArgs
          
    _ -> error $ "no command named " <> x <> " seems to exist"
execute _ = help

