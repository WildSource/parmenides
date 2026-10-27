{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Dialect.Postgresql where

import Data.Int (Int64)
import Data.Word (Word16)
import System.Environment (getEnv)
import Control.Exception (try)

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..)
  , Connection
  , Query
  , SqlError
  , Only (..)
  , executeMany
  , execute
  , execute_
  , query_
  , withConnect
  , withTransaction
  )

import Dialect.Internal (Migration (..))  

-- TODO Apply Reader Monad to these implementation to reduce boilerplate on Connection handling
getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- getEnv "PARMENIDES_HOST"
  port <- getEnv "PARMENIDES_PORT"
  user <- getEnv "PARMENIDES_USER"
  password <- getEnv "PARMENIDES_PASSWORD"
  name <- getEnv "PARMENIDES_NAME"

  pure $ ConnectInfo
    host
    (read port :: Word16)
    user
    password
    name

-- Does not perform parameter substitution
executeStatement :: Query -> IO Int64
executeStatement statement =
  let statement' = flip execute_ statement
  in  getConnectInfo >>= flip withConnect (\ conn -> withTransaction conn $ statement' conn)

createMigrationTrackingTable :: IO Int64
createMigrationTrackingTable =
  let statement :: Connection -> IO Int64
      statement conn = execute_ conn [sql|
                                         CREATE TABLE IF NOT EXISTS parmenides_migration (
                                         id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
                                         filename VARCHAR(50) NOT NULL,
                                         filepath VARCHAR(50) NOT NULL,
                                         version BIGINT NOT NULL,
                                         applied BOOLEAN NOT NULL
                                         )
                                         |]
  in  getConnectInfo >>= flip withConnect (\ conn -> withTransaction conn $ statement conn)

insertMigrationQuery :: Query
insertMigrationQuery =
  [sql|
      INSERT INTO parmenides_migration (filename, filepath, version, applied)
      VALUES (?, ?, ?, ?)
      |]
    
createTrackedMigration
  :: String
  -> String
  -> String
  -> IO (Either SqlError Int64)
createTrackedMigration filename filepath version =
  let statement :: Connection -> IO (Either SqlError Int64)
      statement conn =
        try $
        execute
        conn
        insertMigrationQuery
        (filename, filepath, version, False)
  in  getConnectInfo >>= flip withConnect (\ conn -> withTransaction conn $ statement conn)

type Filename = String
type Filepath = String
type Version = String
createTrackedMigrations
  :: [(Filename, Filepath, Version, Bool)]
  -> IO (Either SqlError Int64)
createTrackedMigrations filedata = do
  connInfo <- getConnectInfo
  withConnect
    connInfo
    (\ conn ->
       withTransaction conn $ try $ 
       executeMany
       conn
       insertMigrationQuery
       filedata
    )

getTrackedMigrationNames :: IO [String]
getTrackedMigrationNames = do
  let statement :: Connection -> IO [String]
      statement =
        (\ conn' -> do
            values <- query_ conn' [sql| SELECT filename FROM parmenides_migration |]
            pure $ fmap (\ (Only filename) -> filename) values
        )
  connInfo <- getConnectInfo
  withConnect connInfo (\ conn -> withTransaction conn (statement conn))

getUnappliedMigrations :: IO [Migration]
getUnappliedMigrations = do
  connInfo <- getConnectInfo
  withConnect connInfo $ \ conn -> do
    migrationTuples <- query_ conn [sql|
                         SELECT filename, version, applied
                         FROM parmenides_migration
                         WHERE applied = FALSE
                         ORDER BY version ASC
                         |]
    pure $ fmap (\ (f, v, a) -> Migration f v a) migrationTuples

-- TODO Implement this function
-- Implementation include transaction rollback
applyMigration :: String -> IO ()
applyMigration filepath =
  let filename = drop 11 filepath
  in putStrLn $ "Applied " ++ filename ++ " to database"

