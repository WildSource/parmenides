
module Main (main) where

import System.Directory
import System.Console.ANSI.Types (Color (Green))
import Log

migrationDir :: String
migrationDir = "./migration"

migrationFolder :: IO ()
migrationFolder = do
  doesExist <- doesDirectoryExist migrationDir
  migrationFolder' doesExist
  where

    migrationFolder' :: Bool -> IO ()
    migrationFolder' True = logInfo "Migration directory exists"
    migrationFolder' False
      =  logWarning "Migration directory missing"
      >> createDirectory migrationDir
      >> putStr "    " >> applyColor Green "create" >> putStr " /migrations\n"
      
main :: IO ()
main = migrationFolder

