{-# LANGUAGE OverloadedStrings #-}
module Dialect.Sqlite where

import System.Environment (getEnv)
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple
  ( open
  , close
  , execute_
  , Query (..)
  , FromRow (..)
  )

doesTrackingTableExist :: IO Bool
doesTrackingTableExist = undefined

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

migrate :: [FilePath] -> IO ()
migrate = mapM_ migrateSqlite' 
  where
    migrateSqlite' :: FilePath -> IO ()
    migrateSqlite' filepath = do
      scriptContent <- TIO.readFile filepath
      url <- getEnv "PARMENIDES_URL" 
      conn <- open url
      execute_ conn (Query scriptContent)
      putStrLn $ "migration " ++ filepath ++ " was ran"
      close conn
