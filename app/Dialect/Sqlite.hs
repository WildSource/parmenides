{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Dialect.Sqlite where

import Data.String (fromString)
import Control.Exception (try)
import Data.Functor (void)

import Database.SQLite.Simple.QQ (sql)
import System.Environment (getEnv)
import Database.SQLite.Simple
  ( Only (..)
  , withConnection
  , withTransaction
  , execute
  , execute_
  , FormatError
  )
import Dialect (Dialect (..))
import Dialect.Types
  ( Migration (..)
  , Filename
  , Filepath
  , Version
  )  

dialect :: Dialect String FormatError
dialect = Dialect
  { getConnection = getConnection'
  , createMigrationTrackingTable = createMigrationTrackingTable'
  , createTrackedMigration = createTrackedMigration'
  , applyMigration = applyMigration'
  }


getConnection' :: IO String
getConnection' = getEnv "PARMENIDES_DB_FILE"

createMigrationTrackingTable' :: String -> IO (Either FormatError ())
createMigrationTrackingTable' connStr =
  withConnection connStr $ \ conn ->
    withTransaction conn $ try $ void $
    execute_ conn
    [sql|
        CREATE TABLE IF NOT EXISTS parmenides_migration (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        filename VARCHAR(50) NOT NULL,
        filepath VARCHAR(50) NOT NULL,
        version BIGINT NOT NULL,
        applied BOOLEAN NOT NULL DEFAULT 0
        )
        |]
    
createTrackedMigration'
  :: String
  -> Filename
  -> Filepath
  -> Version
  -> IO (Either FormatError ())
createTrackedMigration' connStr filename filepath version =
  withConnection connStr $ \ conn ->
    withTransaction conn $ try $ void $ execute conn
    [sql|
        INSERT INTO parmenides_migration (filename, filepath, version, applied)
        VALUES (?, ?, ?, ?)
        |]
      (filename, filepath, version, False)

applyMigration' :: String -> Migration -> IO (Either FormatError ())
applyMigration' connStr migration = do
  withConnection connStr $ \ conn -> try $ do
    queryStr <- readFile $ filepath migration
    execute_ conn $ fromString queryStr

markApplied' :: String -> Int -> IO (Either FormatError ())
markApplied' connStr identifier = do
  withConnection connStr $ \ conn -> try $ void $
    execute
    conn
    [sql|
        UPDATE parmenides_migration
        SET applied = 1
        WHERE id = ?
        |]
      (Only identifier)

