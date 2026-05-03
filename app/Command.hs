module Command where

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
createMigration title = undefined

runMigrations :: Flavor -> [String] -> IO ()
runMigrations = undefined
