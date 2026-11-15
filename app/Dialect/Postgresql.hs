{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Dialect.Postgresql where

import Data.Functor (void)
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

import qualified Database.PostgreSQL.Simple as PostgreSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..)
  , Connection
  , Query
  , SqlError
  , Only (..)
  , execute
  , execute_
  , query_
  , withConnect
  , withTransaction
  )

import Dialect (Dialect (..))
import Dialect.Types
  ( Migration (..)
  , Filename
  , Filepath
  , Version
  )  

dialect :: Dialect ConnectInfo Connection SqlError
dialect = Dialect
  { getConnectionInfo = getConnectInfo'
  , withConnection = PostgreSQL.withConnect
  , Dialect.withTransaction = PostgreSQL.withTransaction
  , createMigrationTrackingTable = createMigrationTrackingTable'
  , createTrackedMigration = createTrackedMigration'
  , applyMigration = applyMigration'
  , markApplied = markApplied'
  , getOrderedUnappliedMigrations = getOrderedUnappliedMigrations'
  , getTrackedMigrationNames = getTrackedMigrationNames'
  }

getConnectInfo' :: IO ConnectInfo
getConnectInfo' = do
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

createMigrationTrackingTable' :: Connection -> IO (Either SqlError ())
createMigrationTrackingTable' conn =
  try $ PostgreSQL.withTransaction conn $ void $
  execute_
  conn
  [sql|
      CREATE TABLE IF NOT EXISTS parmenides_migration (
      id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      filename VARCHAR(50) NOT NULL,
      filepath VARCHAR(50) NOT NULL,
      version BIGINT NOT NULL,
      applied BOOLEAN NOT NULL DEFAULT FALSE
      )
      |]
    
createTrackedMigration'
  :: Connection
  -> Filename
  -> Filepath
  -> Version
  -> IO (Either SqlError ())
createTrackedMigration' conn filename filepath version =
  PostgreSQL.withTransaction conn $ try $ void $
  execute
  conn
  [sql|
      INSERT INTO parmenides_migration (filename, filepath, version, applied)
      VALUES (?, ?, ?, ?)
      |]
    (filename, filepath, version, False)

{- TODO use this when implementing migrate command
  connInfo <- getConnectInfo
  withConnect connInfo $ \ conn ->
    runExceptT $ traverse_ (ExceptT . applyMigration conn) migrations
-}

applyMigration' :: Connection -> Migration -> IO (Either SqlError ())
applyMigration' conn (Migration { filename, filepath }) = do
  try $ PostgreSQL.withTransaction conn $ do
    queryStr <- readFile filepath
    void $ execute_ conn $ fromString queryStr
    putStrLn $ "Migration " ++ filename ++ " has been successfully applied"

markApplied' :: Connection -> Int -> IO (Either SqlError ())
markApplied' connInfo identifier = try $ void $
  execute
  connInfo
  [sql|
      UPDATE parmenides_migration
      SET applied = TRUE
      WHERE id = ?
      |]
    (Only identifier)

getOrderedUnappliedMigrations' :: Connection -> IO [Migration]
getOrderedUnappliedMigrations' conn = do
  migrationTuples <- query_ conn
    [sql|
        SELECT *
        FROM parmenides_migration
        WHERE applied = FALSE
        ORDER BY version ASC
        |]
  pure $ fmap (\ (i, fn, fp, v, a) -> Migration i fn fp v a) migrationTuples
  
getTrackedMigrationNames' :: Connection -> IO [String]
getTrackedMigrationNames' conn = do
  values <- query_ conn [sql| SELECT filename FROM parmenides_migration |]
  pure $ fmap (\ (Only filename) -> filename) values
  
