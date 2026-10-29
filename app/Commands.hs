module Commands where

import Data.Foldable (traverse_)
import Data.List
  ( (\\)
  , sort
  )
import System.Directory
  ( removeFile
  , getDirectoryContents
  )
import qualified Data.List.NonEmpty as NE
import Control.Monad
  ( when
  , unless
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  )

import Text.PrettyPrint
  (($$)
  , render
  , text
  , vcat
  , Doc
  )
import Database.PostgreSQL.Simple (SqlError (..))
import System.Console.ANSI (Color(Red))

import Log (applyColor)
import Utility (getTimestamp)
import Dialect.Postgresql
  ( createMigrationTrackingTable
  , createTrackedMigration
  , createTrackedMigrations  
  , getTrackedMigrationNames
  )
import Command
  ( Command (..)
  , mkHelpDoc
  )
import Dialect.Internal (Migration (..))  
import Dialect.Postgresql
  ( applyMigrations
  , getUnappliedMigrations
  )

commands :: [Command]
commands =
  [ helpCommand
  , setupCommand
  , setupTrackingCommand
  , mkMigrationCommand
  , findMigrations
  , migrateCommand
  ]

helpCommand :: Command
helpCommand = 
  Command
  { name = "help"
  , run = (\ args -> do
              verifyArguments args
              unless (not (null args)) helpInternal
          )
  , helpDoc = mkHelpDoc "help" [] "This is the help command" 
  }

helpInternal :: IO ()
helpInternal =
  let helpDocs = fmap (helpDoc) commands
      usage = text "Usage: parmenides CMD [ARG ...]"
      help' = text "Commands:" $$ vcat helpDocs
  in 
    putStrLn $ render $ usage $$ help'  

setupCommand :: Command
setupCommand = 
  Command
  { name = "setup"
  , run = (\ args -> do
              verifyArguments args
              unless (not (null args)) setupInternal
          )
  , helpDoc =
      mkHelpDoc
      "setup"
      []
      "Creates a migrations/ directory and an environment file for parmenides"
  }

setupInternal :: IO ()
setupInternal = do
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
      text "PARMENIDES_PASSWORD=" $$
      text " "

setupTrackingCommand :: Command
setupTrackingCommand =
  Command
  { name = "setupTracking"
  , run = (\ args -> do
              verifyArguments args
              unless (not (null args)) setupTrackingInternal
          )
  , helpDoc =
      mkHelpDoc
      "setupTracking"
      []
      "Create a table in the database to track migrations"
  }

setupTrackingInternal :: IO ()
setupTrackingInternal =
  let eventLog = "Migration tracking table has been created and committed to the database"
  in  createMigrationTrackingTable
      >> putStrLn eventLog      

mkMigrationCommand :: Command
mkMigrationCommand =
  Command
  { name = "mkMigration"
  , run =
      (\ args ->
          maybe
          (errorCommand "Command \"mkMigration\" was given insufficient arguments for execution")
          (mkMigrationInternal . NE.head)
          (NE.nonEmpty args))
  , helpDoc =
      mkHelpDoc
      "mkMigration"
      ["title"]
      "Creates a migration file to migrations/ and tracks it"
  }

mkMigrationInternal :: String -> IO ()
mkMigrationInternal title = do
  timestamp <- getTimestamp

  let timestampStr = show timestamp
  let filename = title ++ "_" ++ timestampStr ++ ".sql"
  let filepath = "migrations/" ++ filename
  
  writeFile filepath ""
  
  result <- createTrackedMigration filename filepath timestampStr
  
  case result of
    Left (err :: SqlError) ->
      removeFile filepath
      >> print err
      >> errorCommand "Failed insertion of tracked migration in database"
      >> pure ()
      
    Right _ -> 
      pure ()

findMigrations :: Command
findMigrations = Command
  { name = "findMigrations"
  , run = (\ args -> do
              verifyArguments args
              unless (not (null args)) findMigrationsInternal
          )
  , helpDoc =
      mkHelpDoc
      "findMigrations"
      []
      "Checks for new migrations that are not tracked and add entries for them"
  }

findMigrationsInternal :: IO ()
findMigrationsInternal = do
  untrackedMigrations <- getUntrackedMigrationInDirectory
  when
    (not $ null untrackedMigrations)
    $ scanMigrations untrackedMigrations
  where
    scanMigrations :: [String] -> IO ()
    scanMigrations untrackedMigrations = do
      let filedataTuple =
            fmap
            (\ filename ->
               let timestampStr = reverse $ take 14 $ drop 4 $ reverse filename 
               in (filename, "migrations/" ++ filename, timestampStr, False))
            $ sort untrackedMigrations
      eitherResult <- createTrackedMigrations filedataTuple
      either
        (\ (e :: SqlError) -> do
            print $ show e
            errorCommand "New insertions of migration entries failed"
        )
        (\ _ -> putStrLn "Adding new migrations to the tracking table")
        eitherResult
        
migrateCommand :: Command
migrateCommand = Command
  { name = "migrate"
  , run = (\ _ -> do
              run findMigrations []
              migrateCommandInternal
          )
  , helpDoc =
      mkHelpDoc
      "migrate"
      []
      "Runs findMigrations first cmd and then runs unapplied migrations"
  }

-- TODO Make applyMigrations safer
migrateCommandInternal :: IO ()
migrateCommandInternal = do
  migrations <- getUnappliedMigrations
  -- affectedRows <- applyMigrations
  -- putStrLn $ "Affected Rows: " ++ affectedRows
  putStrLn "In progress"
 
getUntrackedMigrationInDirectory :: IO [String]
getUntrackedMigrationInDirectory = do
  migrationsInDir <- getMigrationNames
  migrationsInDb <- getTrackedMigrationNames
  pure $ migrationsInDir \\ migrationsInDb

getMigrationNames :: IO [String]
getMigrationNames = do
  dirContent <- getDirectoryContents "migrations"
  pure $ filter (\ migration -> migration /= "." && migration /= "..") dirContent  
  
errorCommand :: String -> IO ()
errorCommand message
  = applyColor Red "Error: "
  >> putStrLn (render (text message))
  >> run helpCommand []

-- Meant to be used with function that requires no arguments to run
verifyArguments :: [String] -> IO ()
verifyArguments args =
  when
  (not (null args))
  (errorCommand "Arguments were provided to a command that doesn't need them")
  
