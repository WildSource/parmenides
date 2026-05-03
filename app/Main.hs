module Main (main) where

import Command
import Data.Maybe (isNothing, fromJust)
import System.Environment (getArgs)
import LoadEnv (loadEnv)
import Data.List.NonEmpty qualified as NE
  ( nonEmpty
  , head
  )

main :: IO ()
main = do
  loadEnv
  args <- getArgs

  let command = NE.nonEmpty args

  if isNothing command
    then help
    else
    case NE.head (fromJust command) of
        "scan" ->  scanMigrations >>= print
        _ -> help

  
