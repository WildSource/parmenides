{-# LANGUAGE OverloadedStrings #-}
module Dialect.Postgresql where

import Data.Int (Int64)
import Data.Word (Word16)
import System.Environment (getEnv)
import Control.Exception (try)

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

-- Does not perform parameter substitution
executeStatement :: Query -> IO Int64
executeStatement statement =
  let statement' = flip execute_ statement
  in  getConnectInfo >>= flip withConnect (\ conn -> withTransaction conn $ statement' conn)

createMigrationTrackingTable :: IO Int64
createMigrationTrackingTable =
  let 
      createMigrationTrackingTable' :: Query
      createMigrationTrackingTable'
        =  "CREATE TABLE IF NOT EXISTS parmenides_migration (\n\
           \  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,\n\
           \  filepath VARCHAR(50) NOT NULL,\n\
           \  applied BOOLEAN NOT NULL\n\
           \);"
           
      statement :: Connection -> IO Int64
      statement conn = execute_ conn createMigrationTrackingTable'
      
  in  getConnectInfo >>= flip withConnect (\ conn -> withTransaction conn $ statement conn)

createTrackedMigration :: String -> IO (Either SqlError Int64)
createTrackedMigration filepath =
  let statement
        = "INSERT INTO parmenides_migration (filepath, applied)\
          \VALUES (?, ?);"
          
      statement' :: Connection -> IO (Either SqlError Int64)
      statement' conn =
        let dbAction = execute conn statement (filepath, False)
        in  try dbAction
      
  in  getConnectInfo >>= flip withConnect (\ conn -> withTransaction conn $ statement' conn)



