{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Dialect.Postgresql where

import Control.Monad.Trans.Except
  ( ExceptT (..)
  , runExceptT
  )
import Data.Foldable (traverse_)
import Data.String (fromString)
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

getOrderedUnappliedMigrations :: IO [Migration]
getOrderedUnappliedMigrations = do
  connInfo <- getConnectInfo
  withConnect connInfo $ \ conn -> do
    migrationTuples <- query_ conn [sql|
                         SELECT *
                         FROM parmenides_migration
                         WHERE applied = FALSE
                         ORDER BY version ASC
                         |]
    pure $ fmap (\ (i, fn, fp, v, a) -> Migration i fn fp v a) migrationTuples

-- TODO make it safer by returning Either
-- Assume migrations are pre-sorted (data should come from getOrderedUnappliedMigrations)
applyMigrations :: [Migration] -> IO (Either SqlError ())
applyMigrations migrations = do
  connInfo <- getConnectInfo
  withConnect connInfo $ \ conn ->
    runExceptT $ traverse_ (ExceptT . applyMigration conn) migrations
  where      
    applyMigration :: Connection -> Migration -> IO (Either SqlError Int64)
    applyMigration conn (Migration { identifier, filename, filepath }) = do
      try $ withTransaction conn $ do
        queryStr <- readFile filepath
        affRow <- execute_ conn $ fromString queryStr
        _ <- makeMigrationStateApplied conn identifier
        putStrLn $ "Migration " ++ filename ++ " has been successfully applied"
        pure affRow

    makeMigrationStateApplied :: Connection -> Int -> IO Int64
    makeMigrationStateApplied conn identifier =
      execute
      conn
      [sql|
          UPDATE parmenides_migration
          SET applied = TRUE
          WHERE id = ?
          |]
        (Only identifier)

