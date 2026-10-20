{-# LANGUAGE OverloadedStrings #-}
module Dialect.Postgresql where

import Data.Int (Int64)
import Data.Word (Word16)
import System.Environment (getEnv)

import Database.PostgreSQL.Simple

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

createMigrationTrackingTable :: IO Int64
createMigrationTrackingTable =
  let 
      createMigrationTrackingTable' :: Query
      createMigrationTrackingTable'
        =  "CREATE TABLE parmenides_migration (\n\
           \  id INT PRIMARY KEY,\n\
           \  filepath VARCHAR(50) NOT NULL,\n\
           \  applied BOOLEAN NOT NULL\n\
           \);"
           
      statement :: Connection -> IO Int64
      statement conn = execute_ conn createMigrationTrackingTable'
      
  in  getConnectInfo >>= flip withConnect (\conn -> withTransaction conn $ statement conn)

