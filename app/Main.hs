
module Main (main) where

import Command
import System.Environment (getArgs)
import LoadEnv (loadEnv)
import Data.List.NonEmpty
  ( NonEmpty (..)
  , nonEmpty
  )

runCommands :: NonEmpty String -> IO ()
runCommands (name :| parameters) =
  case name of
    "scan"   -> scanMigrations >>= print
    "dir"    -> migrationFolder
    "create" -> createMigration $ head parameters
    _        -> help

main :: IO ()
main = do
  loadEnv
  args <- getArgs
  let command = nonEmpty args :: Maybe (NonEmpty String)
  maybe help runCommands command

