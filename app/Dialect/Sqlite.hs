{-# LANGUAGE OverloadedStrings #-}
module Dialect.Sqlite where

import Data.Int (Int64)

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

import Dialect.Internal
  ( Migration (..)
  , Filename
  , Filepath
  , Version
  )  

-- TODO takes file db path from env file
getConnection :: IO Connection
getConnection = undefined

createMigrationTrackingTable :: IO Int64
createMigrationTrackingTable = undefined

insertMigrationQuery :: Query
insertMigrationQuery = undefined

createTrackedMigration
  :: String
  -> String
  -> String
  -> IO (Either SqlError Int64)
createTrackedMigration = undefined  

createTrackedMigrations
  :: [(Filename, Filepath, Version, Bool)]
  -> IO (Either SqlError Int64)
createTrackedMigrations = undefined  

getTrackedMigrationNames :: IO [String]
getTrackedMigrationNames = undefined

getOrderedUnappliedMigrations :: IO [Migration]
getOrderedUnappliedMigrations = undefined

applyMigrations :: [Migration] -> IO (Either SqlError ())
applyMigrations = undefined
