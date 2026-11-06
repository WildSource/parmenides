{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Command where

import System.Environment (getEnv)
import Data.List.NonEmpty
  ( NonEmpty
  , nonEmpty
  )
import Data.Char (toUpper)
import Control.Monad.Reader
  ( ReaderT
  , runReaderT
  )

import LoadEnv (loadEnvFrom)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint
  ( nest
  , render
  , text
  , (<+>)
  , Doc
  )

import qualified Dialect as Dialect
import Dialect (Dialect (..))
import Dialect.Flavor (mkFlavor)
import Log (reportError)  

data Commands = Commands
  { helpCmd :: Command
  , setupCmd :: Command
  , setupTrackingCmd :: Command
  , mkMigrationCmd :: Command
  , migrateCmd :: Command
  }

{-
The function stored in 'run' parses the args then runs their implementation and
reports errors themselves using the Log.reportError function.

The program can't easily generalize the process of parsing arguments because
it's extremely variant.

A function could run with or without args or make database interactions and
the type machinery to handle that and make it 100% type safe complicates the code way too much.
-}
data Command
  = Command
  { name :: String
  , run :: [String] -> IO () 
  , helpDoc :: Doc
  }

executeCommand :: Command -> [String] -> IO ()
executeCommand (Command _ run _) args = run args

parseArgs :: [String] -> Either (IO ()) (NonEmpty String)
parseArgs args =
  case nonEmpty args of
    Nothing ->
      Left $ reportError "No arguments has been provided for a command that needs them"
    Just nonEmptyArgs ->
      Right nonEmptyArgs

validateArgs :: NonEmpty String -> [String] -> Either (IO ()) (NonEmpty String)
validateArgs args options = do
  let conditions = fmap (flip elem options) args
  case and conditions of
    False ->
      Left $ reportError $ "An argument provided is not a valid option\n  Try " ++ show options
    True ->
      Right args

toList :: Commands -> [Command]
toList cmd =
  [ cmd.helpCmd
  , cmd.setupCmd
  , cmd.setupTrackingCmd
  , cmd.mkMigrationCmd
  , cmd.migrateCmd
  ]    

instance Show Command where
  show (Command name _ helpDoc) =
    "Command name = " ++ name ++ ", helpDoc = " ++ render helpDoc

-- HELPER FUNCTIONS AND VALUES
  
startingGap :: Int
startingGap = 2

characterLimit :: Int
characterLimit = 20

dynamicGap :: Int -> String -> String
dynamicGap charLimit word =
  replicate (charLimit - (Prelude.length word)) ' '

mkHelpDoc :: String -> [String] -> String -> Doc
mkHelpDoc name [] msg =
  let name' = nest startingGap (text name)
      gap = dynamicGap characterLimit name
  in  name' PP.<> (text (gap ++ msg))
mkHelpDoc name args msg =
  let name' = nest startingGap $ text name
      gap = dynamicGap characterLimit (name ++ " " ++ show args')
      args' = text $ renderArgs args
  in  name' <+> args' PP.<> (text (gap ++ msg))
  where
    renderArgs :: [String] -> String
    renderArgs [] = []
    renderArgs (x : xs) =
      let arg = fmap toUpper x
      in  arg ++ " " ++ renderArgs xs

