{-# LANGUAGE OverloadedRecordDot  #-} 
module Commands where

import Control.Exception (Exception)
import Control.Monad (void)
import Data.Foldable (traverse_)  
import Control.Exception (displayException)
import System.Environment (getEnv)
import Data.List
  ( (\\)
  , find
  )
import Data.List.NonEmpty
  ( NonEmpty
  , nonEmpty
  )
import qualified Data.List.NonEmpty as NE
import Control.Monad
  ( when
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeFile
  , getDirectoryContents
  )

import Text.PrettyPrint
  (($$)
  , render
  , text
  , vcat
  , Doc
  )
import Database.PostgreSQL.Simple
  ( FormatError (..)
  , SqlError (..)
  )

import Utility (getTimestamp)
import Log (reportError)
import qualified Dialect as Dialect
import Dialect
  ( Dialect (..)
  , SomeDialect (..)
  )
import Dialect.Types (Migration)  
import Dialect.Flavor
  ( Flavor (..)
  , mkFlavor
  )  
import qualified Dialect.Sqlite as DS
import qualified Dialect.Postgresql as DP
import qualified Command as Command  
import Command
  ( Command (..)
  , Commands (..)
  , mkHelpDoc
  )

commands :: Commands
commands = Commands
  { helpCmd = helpCommand
  , setupCmd = setupCommand
  , setupTrackingCmd = setupTrackingCommand
  , mkMigrationCmd = mkMigrationCommand
  , migrateCmd = migrateCommand
  }

helpCommand :: Command
helpCommand = 
  Command
  { name = "help"
  , run = (\ _ -> helpInternal)
  , helpDoc = mkHelpDoc "help" [] "This is the help command" 
  }
  where
    helpInternal :: IO ()
    helpInternal =
      let helpDocs = fmap (helpDoc) $ Command.toList commands
          usage = text "Usage: parmenides CMD [ARG ...]"
          help' = text "Commands:" $$ vcat helpDocs
      in  putStrLn $ render $ usage $$ help'  

setupCommand :: Command
setupCommand = 
  Command
  { name = "setup"
  , run = (either id setupInternal . Command.parseArgs)
  , helpDoc =
      mkHelpDoc
      "setup"
      ["database"]
      "Creates a migrations/ dir and a db specific env file for parmenides"
  }
  where
    setupInternal :: NonEmpty String -> IO ()
    setupInternal flavorStr = do
      dirExist <- doesDirectoryExist "migrations"
      when (not dirExist) createMigrationDirectory

      fileExist <- doesFileExist "parmenides.env"
      case mkFlavor $ NE.head flavorStr of
        Nothing ->
          reportError $
          "Database flavor in env file is wrong, try \"sqlite\", \"postgresql\" or \"mysql\""
        Just flavor -> 
          when (not fileExist) $ createParmenidesEnvironmentFile flavor

    createMigrationDirectory :: IO ()
    createMigrationDirectory =
      let dirName = "migrations"
      in  createDirectoryIfMissing True dirName
      >>  putStrLn (dirName ++ "/ directory created")

    createParmenidesEnvironmentFile :: Flavor -> IO ()
    createParmenidesEnvironmentFile flavor =
      let filename = "parmenides.env"
          generatedEnvFile =
            case flavor of
              SQLite -> envFileSqlite
              PostgreSQL -> envFilePostgresql
              MySQL -> envFileMySql
      in writeFile filename ((render generatedEnvFile) ++ "\n")
      >> addToGitignore filename
      >> putStrLn (filename ++ " environment file created")

    addToGitignore :: String -> IO ()
    addToGitignore filepath = do
      ignoreExist <- doesFileExist ".gitignore"
      when ignoreExist $ appendFile ".gitignore" $ "\n" ++ filepath
      
    envFilePostgresql :: Doc
    envFilePostgresql =
      text "PARMENIDES_FLAVOR=postgresql" $$
      text "PARMENIDES_HOST=" $$
      text "PARMENIDES_PORT=" $$
      text "PARMENIDES_USER=" $$
      text "PARMENIDES_NAME=" $$
      text "PARMENIDES_PASSWORD=\n"

    envFileSqlite :: Doc
    envFileSqlite =
      text "PARMENIDES_FLAVOR=sqlite" $$
      text "PARMENIDES_DB_FILE=\n"

    -- TODO env file for Mysql (by extension Mariadb)
    envFileMySql :: Doc
    envFileMySql = undefined

setupTrackingCommand :: Command
setupTrackingCommand =
  Command
  { name = "setupTracking"
  , run = (\_ -> setupTrackingInternal)
  , helpDoc =
      mkHelpDoc
      "setupTracking"
      []
      "Create a table in the database to track migrations"
  }

setupTrackingInternal :: IO ()
setupTrackingInternal = do
  someDialect <- getDialect
  Dialect.withSomeDialect someDialect $ \ dialect -> do
    connInfo <- dialect.getConnectionInfo
    dialect.withConnection connInfo $ \ conn -> do
      result <- dialect.createMigrationTrackingTable conn
      case result of
        Left err ->
          reportError "Setting Tracking table failed"
          >> putStrLn (displayException err)
        Right _ ->
          putStrLn "Migration tracking table has been created and committed to the database"

mkMigrationCommand :: Command
mkMigrationCommand =
  Command
  { name = "mkMigration"
  , run = (either id mkMigrationInternal . Command.parseArgs)
  , helpDoc =
      mkHelpDoc
      "mkMigration"
      ["title"]
      "Creates a migration file to migrations/ and tracks it"
  }

mkMigrationInternal :: NonEmpty String -> IO ()
mkMigrationInternal title = do
  someDialect <- getDialect
  Dialect.withSomeDialect someDialect $ \ dialect -> do
    connInfo <- dialect.getConnectionInfo
    dialect.withConnection connInfo $ \ conn -> do
      timestamp <- getTimestamp

      let timestampStr = show timestamp
      let filename = NE.head title ++ "_" ++ timestampStr ++ ".sql"
      let filepath = "migrations/" ++ filename
  
      writeFile filepath ""
  
      result <- dialect.createTrackedMigration conn filename filepath timestampStr
  
      case result of
        Left err ->
          removeFile filepath
          >> reportError "Failed insertion of tracked migration in database"
          >> putStrLn (displayException err)
          >> pure ()
      
        Right _ -> 
          pure ()

migrateCommand :: Command
migrateCommand = Command
  { name = "migrate"
  , run = (\ _ -> migrateCommandInternal)
  , helpDoc =
      mkHelpDoc
      "migrate"
      []
      "Runs unapplied migrations tracked by Parmenides"
  }

migrateCommandInternal :: IO ()
migrateCommandInternal = do
  someDialect <- getDialect
  
  void $ Dialect.withSomeDialect someDialect $ \ dialect -> do
    connInfo <- dialect.getConnectionInfo    
    dialect.withConnection connInfo $ \ conn -> do
      migrations <- dialect.getOrderedUnappliedMigrations conn
      traverse_ (applyMigration conn dialect) migrations
  where
    applyMigration
      :: Exception e
      => c
      -> Dialect i c e
      -> Migration
      -> IO ()
    applyMigration conn dialect migration =
      dialect.withTransaction conn $ do
        result <- dialect.applyMigration conn migration
        either
          (\ e -> do
              putStrLn $ displayException e
              reportError "A migration failed to apply"
          )
          (\ _ -> putStrLn "Migrations have been applied successfully")
          result
        pure ()

-- Helper functions

getCmd :: String -> Maybe Command
getCmd name' = find (\ cmd -> name cmd == name') $ Command.toList commands

getDialect :: IO SomeDialect
getDialect = do
  flavorStr <- getEnv "PARMENIDES_FLAVOR"
  case mkFlavor flavorStr of
    Nothing -> undefined
    Just flavor ->
      case flavor of 
        SQLite -> pure $ SomeDialect DS.dialect
        PostgreSQL -> pure $ SomeDialect DP.dialect
        MySQL -> undefined
        
