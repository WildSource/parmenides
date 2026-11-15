{-|
Module: Dialect.Sqlite
Description: Contains the implementation of Sqlite's db interaction
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module contains the low level implementation of database interactions for the Sqlite database specifically.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Dialect.Sqlite where

-- Prelude imports
import Data.String (fromString)
import Control.Exception (try)
import Data.Functor (void)

-- Library imports
import Database.SQLite.Simple.QQ (sql)
import System.Environment (getEnv)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple
  ( Only (..)
  , Connection
  , execute
  , execute_
  , query_
  , FormatError
  )
import Dialect (Dialect (..))
import Dialect.Types
  ( Migration (..)
  , Filename
  , Filepath
  , Version
  )  

dialect :: Dialect String Connection FormatError
dialect = Dialect
  { getConnectionInfo = getConnectionInfo'
  , Dialect.withConnection = SQLite.withConnection
  , Dialect.withTransaction = SQLite.withTransaction
  , createMigrationTrackingTable = createMigrationTrackingTable'
  , createTrackedMigration = createTrackedMigration'
  , applyMigration = applyMigration'
  , markApplied = markApplied'
  , getOrderedUnappliedMigrations = getOrderedUnappliedMigrations'
  , getTrackedMigrationNames = getTrackedMigrationNames'
  }

getConnectionInfo' :: IO String
getConnectionInfo' = getEnv "PARMENIDES_DB_FILE"

createMigrationTrackingTable' :: Connection -> IO (Either FormatError ())
createMigrationTrackingTable' conn =
  try $ void $
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
  :: Connection
  -> Filename
  -> Filepath
  -> Version
  -> IO (Either FormatError ())
createTrackedMigration' conn filename filepath version =
  try $ void $ execute conn
  [sql|
      INSERT INTO parmenides_migration (filename, filepath, version, applied)
      VALUES (?, ?, ?, ?)
      |]
    (filename, filepath, version, False)

applyMigration' :: Connection -> Migration -> IO (Either FormatError ())
applyMigration' conn migration = do
  queryStr <- readFile $ filepath migration
  try $ execute_ conn $ fromString queryStr

markApplied' :: Connection -> Int -> IO (Either FormatError ())
markApplied' conn identifier = do
  try $ void $
    execute
    conn
    [sql|
        UPDATE parmenides_migration
        SET applied = 1
        WHERE id = ?
        |]
      (Only identifier)

getOrderedUnappliedMigrations' :: Connection -> IO [Migration]    
getOrderedUnappliedMigrations' conn = do
  migrationTuples <- query_ conn
    [sql|
        SELECT *
        FROM parmenides_migration
        WHERE applied = 0
        ORDER BY version ASC
        |]
  pure $ fmap (\ (i, fn, fp, v, a) -> Migration i fn fp v a) migrationTuples

getTrackedMigrationNames' :: Connection -> IO [String]
getTrackedMigrationNames' conn = do
  values <- query_ conn [sql| SELECT filename FROM parmenides_migration |]
  pure $ fmap (\ (Only filename) -> filename) values

