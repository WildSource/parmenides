{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where

import Prelude hiding ((<>))
import Control.Monad (when)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  )

import System.Console.ANSI (Color(Red))
import Text.PrettyPrint
  (($$)
  , (<>)
  , nest
  , render
  , text
  , Doc
  )

import Log (applyColor)
import Utility (getTimestamp)

help :: IO ()
help =   
  putStrLn $ render $ help'
  where
    help' :: Doc
    help' =
      let startingGap = 4      
          characterLimit = 15
          help = nest startingGap (text "help") <> (text ((dynamicGap characterLimit "help") ++ "The current output is this command"))
          setup = nest startingGap (text "setup") <> (text ((dynamicGap characterLimit "setup") ++ "Creates a migrations/ directory and an environment file for pamenides"))
          mkMigration = nest startingGap (text "mkMigration") <> (text ((dynamicGap characterLimit "mkMigration") ++ "Creates a migration file to migrations/ with a title and a timestamp seperated by \'_\'"))
      in  text "Commands:" $$ help $$ setup $$ mkMigration

    dynamicGap :: Int -> String -> String
    dynamicGap characterLimit word =
      replicate (characterLimit - (length word)) ' '

setup :: IO ()
setup = do
  dirExist <- doesDirectoryExist "migrations"
  when (not dirExist) createMigrationDirectory

  fileExist <- doesFileExist "parmenides.env"
  when (not fileExist) createParmenidesEnvironmentFile
  where
    createMigrationDirectory :: IO ()
    createMigrationDirectory
      =  createDirectoryIfMissing True "migrations"
      >>  putStrLn "migrations/ directory created"

    createParmenidesEnvironmentFile :: IO ()
    createParmenidesEnvironmentFile
      =  writeFile "parmenides.env" (render envFile)  
      >> putStrLn "parmenides.env environment file created"
      
    -- might need to handle other database other than Postgresql in the future
    envFile :: Doc
    envFile =
      text "PARMENIDES_HOST=" $$
      text "PARMENIDES_PORT=" $$
      text "PARMENIDES_USER=" $$
      text "PARMENIDES_NAME=" $$
      text "PARMENIDES_PASSWORD="

mkMigration :: String -> IO ()
mkMigration title = do
  timestamp <- getTimestamp
  let filename = title ++ "_" ++ show timestamp ++ ".sql"
  writeFile ("migrations/" ++ filename) ""
  
error :: String -> IO ()
error message
  = applyColor Red "Error: "
  >> putStrLn (render (text message))
  >> help
