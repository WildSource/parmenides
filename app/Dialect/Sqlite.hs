{-# LANGUAGE OverloadedStrings #-}
module Dialect.Sqlite where

import Log (logError, logRun)
import Control.Monad (mapM, unless)
import Data.Functor ((<&>))
import System.Environment (getEnv)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple
  ( open
  , close
  , execute_
  , query_
  , Query (..)
  , Only (..)
  , FromRow (..)
  )

trackingTableExist :: IO Bool
trackingTableExist =
  getTables <&> elem "parmenides_tracking_table"
  where
    getTables :: IO [String]
    getTables = do
      url <- getEnv "PARMENIDES_URL"
      conn <- open url
      rows <- query_ conn selectTablesStmt
      pure $ map (\ (Only tableName) -> tableName :: String) rows
      where
        selectTablesStmt = "SELECT name \
                           \FROM sqlite_master \
                           \WHERE type='table' \
                           \AND name NOT LIKE 'sqlite_%'"
    
createTrackingTable :: IO ()
createTrackingTable = do
  conn <- getEnv "PARMENIDES_URL" >>= open 
  execute_ conn createTrackingTableStmt
  close conn
  where
    createTrackingTableStmt = "CREATE TABLE IF NOT EXISTS parmenides_tracking_table (\
                              \id INTEGER PRIMARY KEY,\                              
                              \path TEXT UNIQUE,\
                              \applied INTEGER\
                              \)"

{-                              
migrate :: [FilePath] -> IO ()
migrate migrations =
  let sortedMigrations = 
  in  trackingTableExist
  >>= flip unless createTrackingTable
  >>  mapM_ migrate' migrations
  where
    migrate' :: FilePath -> IO ()
    migrate' filepath = do
      scriptContent <- TIO.readFile filepath
      if T.null scriptContent
        then logError $ "Migration " ++ filepath ++ " is empty"
        else do
          url <- getEnv "PARMENIDES_URL" 
          conn <- open url
          execute_ conn (Query scriptContent)
          logRun filepath
          close conn
-}
