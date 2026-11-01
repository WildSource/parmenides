{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Dialect.Sqlite where

import Database.SQLite.Simple.QQ (sql)
import System.Environment (getEnv)
import Database.SQLite.Simple
  ( withConnection
  , withTransaction
  , execute_
  , Query
  , FormatError
  )

import Dialect.Internal
  ( Migration
  , Filename
  , Filepath
  , Version
  )  

getConnection :: IO String
getConnection = getEnv "PARMENIDES_DB_FILE"

-- TODO make commands read env file for db flavor
createMigrationTrackingTable :: IO ()
createMigrationTrackingTable = do
  connPath <- getConnection 
  withConnection connPath $ \ conn ->
    withTransaction conn $ execute_ conn
    [sql|
        CREATE TABLE IF NOT EXISTS parmenides_migration (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        filename VARCHAR(50) NOT NULL,
        filepath VARCHAR(50) NOT NULL,
        version BIGINT NOT NULL,
        applied BOOLEAN NOT NULL
        )
        |]
    

insertMigrationQuery :: Query
insertMigrationQuery = undefined

createTrackedMigration
  :: String
  -> String
  -> String
  -> IO (Either FormatError ())
createTrackedMigration = undefined  

createTrackedMigrations
  :: [(Filename, Filepath, Version, Bool)]
  -> IO (Either FormatError ())
createTrackedMigrations = undefined  

getTrackedMigrationNames :: IO [String]
getTrackedMigrationNames = undefined

getOrderedUnappliedMigrations :: IO [Migration]
getOrderedUnappliedMigrations = undefined

applyMigrations :: [Migration] -> IO (Either FormatError ())
applyMigrations = undefined
