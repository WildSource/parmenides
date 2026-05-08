module Main (main) where

import Log (logError)
import Control.Monad (void)
import Command
import System.Environment (getArgs, getEnv)
import LoadEnv (loadEnv)
import Environment (getFlavor)
import Data.List.NonEmpty qualified as NE
  ( NonEmpty (..)
  , nonEmpty
  , head
  )

runCommands :: NE.NonEmpty String -> IO ()
runCommands (name NE.:| parameters) =
  void $ maybe (zeroArgCommand name) (oneArgCommand name . NE.head) (NE.nonEmpty parameters)
  where
    zeroArgCommand :: String -> IO ()
    zeroArgCommand name' = 
      case name' of
        "scan"    -> scanMigrations >>= print
        "dir"     -> migrationFolder
        "migrate" -> getEnv "PARMENIDES_FLAVOR" >>= (runMigrations . getFlavor)
        _         -> logError ("Stopped at 0 argument commands. \nDid you forget to add a parameter for " ++ name' ++ " ?")

    oneArgCommand :: String -> String -> IO ()
    oneArgCommand name'' arg = 
      case name'' of
        "make" -> createMigration arg
        _        -> logError ("Stopped at 1 argument commands. \nTry adding the more parameters for " ++ name'' ++ " ?")

main :: IO ()
main = do
  loadEnv
  args <- getArgs
  let command = NE.nonEmpty args :: Maybe (NE.NonEmpty String)
  maybe help runCommands command

