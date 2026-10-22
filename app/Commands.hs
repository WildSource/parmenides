module Commands where

import System.Directory (removeFile)
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

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint
  (($$)
  , nest
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
  )
import Command
  ( Command (..)
  , startingGap
  , characterLimit
  , dynamicGap
  )

commands :: [Command]
commands =
  [ helpCommand
  , setupCommand
  , setupTrackingCommand
  , mkMigrationCommand
  ]

helpCommand :: Command
helpCommand = 
  let gap = (dynamicGap characterLimit "help")
  in
    Command
    { name = "help"
    , run = (\ args -> do
               verifyArguments args
               unless (not (null args)) helpInternal
            )
    , helpDoc = nest startingGap (text "help") PP.<> (text (gap ++ "This is the help command"))
    }

helpInternal :: IO ()
helpInternal =
  let helpDocs = fmap (helpDoc) commands
      help' = text "Commands:" $$ vcat helpDocs
  in 
    putStrLn $ render $ help'  

setupCommand :: Command
setupCommand = 
  let helpDoc =
        let gap = (dynamicGap characterLimit "setup")
        in  nest startingGap (text "setup") PP.<>
            (text (gap ++ "Creates a migrations/ directory and an environment file for parmenides"))
  in
    Command
    { name = "setup"
    , run = (\ args -> do
                verifyArguments args
                unless (not (null args)) setupInternal
            )
    , helpDoc = helpDoc
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
      text "PARMENIDES_PASSWORD=\n"

setupTrackingCommand :: Command
setupTrackingCommand =
  let helpDoc =
        let gap = dynamicGap characterLimit "setupTracking"
        in  nest startingGap (text "setupTracking") PP.<>
            (text (gap ++ "Create a table in the database to track migrations"))
  in Command
     { name = "setupTracking"
     , run = (\ args -> do
                 verifyArguments args
                 unless (not (null args)) setupTrackingInternal
             )
     , helpDoc = helpDoc
     }

setupTrackingInternal :: IO ()
setupTrackingInternal =
  let eventLog = "Migration tracking table has been created and committed to the database"
  in  createMigrationTrackingTable
      >> putStrLn eventLog      

mkMigrationCommand :: Command
mkMigrationCommand =
  let helpDoc =
        let gap = (dynamicGap characterLimit "mkMigration")
        in  nest startingGap (text "mkMigration")
            PP.<> (text (gap ++ "Creates a migration file to migrations/ with a title and a timestamp seperated by \'_\'"))
    in Command
       { name = "mkMigration"
       , run =
           (\ args ->
              maybe
              (errorCommand "Command \"mkMigration\" was given insufficient arguments for execution")
              (mkMigrationInternal . NE.head)
              (NE.nonEmpty args))
       , helpDoc = helpDoc
       }

mkMigrationInternal :: String -> IO ()
mkMigrationInternal title = do
  timestamp <- getTimestamp
  let filepath = "migrations/" ++ title ++ "_" ++ show timestamp ++ ".sql"
  writeFile filepath ""
  result <- createTrackedMigration filepath
  case result of
    Left (err :: SqlError) ->
      removeFile filepath
      >> print err
      >> errorCommand "Failed insertion of tracked migration in database"
      >> pure ()
    Right _ -> 
      pure ()

migrate :: Command
migrate = undefined
  
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
