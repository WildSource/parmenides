module Main (main) where

-- From hidden packages
import System.Directory (
  createDirectory
  , doesDirectoryExist
  , listDirectory
  )
import System.FilePath.Posix (takeExtension)
import System.Environment (getEnv)
import Data.List (partition)
import Control.Monad (unless)

-- From libraries
import LoadEnv (loadEnv)
import System.Console.ANSI.Types (Color (Green))

-- From internal
import Log (applyColor, logInfo, logWarning)

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

scanMigrations :: IO [String]
scanMigrations = do
  files <- listDirectory migrationDir
  
  let (migrations, otherFiles) =
        partition (\ file -> ".sql" == takeExtension file) files
        
  unless (null otherFiles)
    $ logWarning $ "These files are not sql scripts " ++ show otherFiles
    
  pure migrations

data Flavor
  = SQLite
  | MySQL
  | PostgreSQL

getFlavorFromString :: String -> Maybe Flavor
getFlavorFromString str =
  case str of
    "sqlite"     -> Just SQLite
    "mysql"      -> Just MySQL
    "postgresql" -> Just PostgreSQL
    _ -> Nothing

getDbFlavor :: IO Flavor
getDbFlavor = do
  

runMigrations :: String -> [String] -> IO ()
runMigrations = undefined
      
main :: IO ()
main = do
  loadEnv
  
  migrationFolder

  migrations <- scanMigrations

  flavor <- getEnv "DB_FLAVOR"

  runMigrations flavor migrations

  print migrations
  
