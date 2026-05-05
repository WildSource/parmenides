{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where

import Database.SQLite.Simple (open, execute_, Query (..))
import System.Environment (getEnv)
import Data.Bool (bool)
import Environment
  ( Flavor (..)
  , migrationDir
  , migrationUpDir
  , migrationDownDir
  )
import System.Directory
  ( createDirectory
  , doesDirectoryExist
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
help = logInfo "This is supposed to be the help function"

migrationFolder :: IO ()
migrationFolder = do
  doesExist <- doesDirectoryExist migrationDir
  if doesExist
    then logInfo "Migration directory exists"
    else logWarning "Migration directory missing"
      >> createMigrationDirectories [migrationDir, migrationUpDir, migrationDownDir]
  where
    createAndLogDirectory :: String -> IO ()
    createAndLogDirectory directoryPath
      = createDirectory directoryPath
      >> putStr "    "
      >> applyColor Green "create"
      >> putStrLn (" " ++ directoryPath)
      
    createMigrationDirectories :: [String] -> IO ()
    createMigrationDirectories = mapM_ createAndLogDirectory

scanMigrations :: IO ([String], [String])
scanMigrations = do
  upFiles <- listDirectory migrationUpDir
  downFiles <- listDirectory migrationDownDir
  let files = upFiles ++ downFiles

  let otherFiles = filter (\ file -> ".sql" /= takeExtension file) files
  
  unless (null otherFiles)
    $ logWarning $ "These files are not sql scripts " ++ show otherFiles
    
  pure (upFiles, downFiles)

createMigration :: String -> IO ()
createMigration title = do
  time <- getTimestamp
  let timestamp = formatTimestamp time 
  let filename = timestamp ++ "_" ++ title ++ ".sql"
  let filepathUp = migrationUpDir ++ "/" ++ filename
  let filepathDown = migrationDownDir ++ "/" ++ filename
  writeFile filepathUp ""
  logCreate filepathUp
  writeFile filepathDown ""
  logCreate filepathDown
  where
    formatTime :: String -> String
    formatTime time = bool time ("0" ++ time) (length time == 1)
              
    formatTimestamp :: Timestamp -> String
    formatTimestamp timestamp =
      let timestamps =
            [ show timestamp.year
            , formatTime $ show timestamp.month
            , formatTime $ show timestamp.day
            , formatTime $ show timestamp.hour
            , formatTime $ show timestamp.minute
            ]
      in concatMap (++ "_") timestamps

migrateSqlite :: [FilePath] -> IO ()
migrateSqlite = mapM_ migrateSqlite' 
  where
    migrateSqlite' :: FilePath -> IO ()
    migrateSqlite' filepath = do
      scriptContent <- TIO.readFile filepath
      url <- getEnv "parmenides_url" 
      conn <- open url
      execute_ conn (Query scriptContent)

migrateMysql :: [FilePath] -> IO ()
migrateMysql scripts = undefined

migratePostgresql :: [FilePath] -> IO ()
migratePostgresql scripts = undefined
     
runMigrations :: Maybe Flavor -> IO ()
runMigrations Nothing = logError "Database Flavor is missing"
runMigrations (Just flavor) =
  let migrations = getUpMigrations
  in
    case flavor of
      SQLite     -> migrations >>= migrateSqlite  
      MySQL      -> migrations >>= migrateMysql 
      PostgreSQL -> migrations >>= migratePostgresql
  where
    getUpMigrations :: IO [FilePath]
    getUpMigrations = do
      let migrationUpDir' = migrationUpDir ++ "/"
      migrationFiles <- listDirectory migrationUpDir
      let scripts = filter (\ file -> ".sql" /= takeExtension file) migrationFiles
      pure $ fmap (migrationUpDir' ++) scripts
     
