{-# LANGUAGE OverloadedRecordDot #-}
module Command where

import Data.Bool
import Environment
  ( Flavor
  , migrationDir
  , migrationUpDir
  , migrationDownDir
  )
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , listDirectory
  )
import Log
  ( applyColor
  , logInfo
  , logWarning
  )
import System.FilePath.Posix (takeExtension)
import System.Console.ANSI.Types (Color (Green))
import Control.Monad (unless)
import Utility hiding (headTail)

help :: IO ()
help = logInfo "This is supposed to be the help function"

migrationFolder :: IO ()
migrationFolder = do
  doesExist <- doesDirectoryExist migrationDir
  if doesExist
    then logInfo "Migration directory exists"
    else logWarning "Migration directory missing"
      >> createMigrationDirectories [migrationDir, migrationUpDir, migrationDownDir]
  where
    createAndLogDirectory :: String -> IO ()
    createAndLogDirectory directoryPath
      = createDirectory directoryPath
      >> putStr "    "
      >> applyColor Green "create"
      >> putStrLn (" " ++ directoryPath)
      
    createMigrationDirectories :: [String] -> IO ()
    createMigrationDirectories = mapM_ createAndLogDirectory

scanMigrations :: IO ([String], [String])
scanMigrations = do
  upFiles <- listDirectory migrationUpDir
  downFiles <- listDirectory migrationDownDir
  let files = upFiles ++ downFiles

  let otherFiles = filter (\ file -> ".sql" /= takeExtension file) files
  
  unless (null otherFiles)
    $ logWarning $ "These files are not sql scripts " ++ show otherFiles
    
  pure (upFiles, downFiles)

createMigration :: String -> IO ()
createMigration title = do
  time <- getTimestamp
  let timestamp = formatTimestamp time 
  let filename = timestamp ++ "_" ++ title ++ ".sql"
  let filepathUp = migrationUpDir ++ "/" ++ filename
  let filepathDown = migrationDownDir ++ "/" ++ filename
  writeFile filepathUp ""
  writeFile filepathDown ""
  where
    formatTime :: String -> String
    formatTime time = bool time ("0" ++ time) (length time == 1)
              
    formatTimestamp :: Timestamp -> String
    formatTimestamp timestamp =
      let timestamps =
            [ show timestamp.year
            , formatTime $ show timestamp.month
            , formatTime $ show timestamp.day
            , formatTime $ show timestamp.hour
            , formatTime $ show timestamp.minute
            ]
      in concatMap (++ "_") timestamps
     

runMigrations :: Flavor -> [String] -> IO ()
runMigrations = undefined

