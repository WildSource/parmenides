{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where

import Prelude hiding ((<>))
import Control.Monad (when)

import Text.PrettyPrint as PP

import Database.SQLite.Simple
  ( open
  , close
  , execute_
  , Query (..)
  )
import System.Environment (getEnv)
import Dialect.Sqlite qualified as SqliteDialect
import Data.Bool (bool)
import Environment
  ( Flavor (..)
  , migrationDir
  , migrationUpDir
  , migrationDownDir
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import Log
  ( applyColor
  , logInfo
  , logWarning
  , logCreate
  , logError
  )
import System.FilePath.Posix (takeExtension)
import System.Console.ANSI.Types (Color (Green))
import Control.Monad (unless)
import Data.Text.IO qualified as TIO
import Utility hiding (headTail)

help :: IO ()
help =   
  print $ render $ help'
  where
    help' :: Doc
    help' =
      text "Commands:" $$
      nest 4 (text "help") <+> text "The current output is this command" $$
      nest 4 (text "setup") <+> text "Creates a migrations/ directory and an environment file for pamenides"

setup :: IO ()
setup = do
  dirExist <- doesDirectoryExist "migrations"
  when (not dirExist) createMigrationDirectory

  fileExist <- doesFileExist "parmenides.env"
  when (not fileExist) createParmenidesEnvironmentFile
  where
    createMigrationDirectory :: IO ()
    createMigrationDirectory
      =  createDirectoryIfMissing True "migrations"
      >>  putStrLn "migrations/ directory created"

    createParmenidesEnvironmentFile :: IO ()
    createParmenidesEnvironmentFile
      =  writeFile "parmenides.env" (render envFile)  
      >> putStrLn "parmenides.env environment file created"
      
    -- might need to handle other database other than Postgresql in the future
    envFile :: Doc
    envFile =
      text "PARMENIDES_HOST=" $$
      text "PARMENIDES_PORT=" $$
      text "PARMENIDES_USER=" $$
      text "PARMENIDES_NAME=" $$
      text "PARMENIDES_PASSWORD="

