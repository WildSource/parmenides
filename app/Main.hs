module Main (main) where

import System.Environment (getArgs)

import Log (reportError)
import Commands (getCmd)
import Command
  ( Command (..)
  , executeCommand
  )

main :: IO ()
main = do
  args <- getArgs
  let parsedCmd = parseCmd args
  case parsedCmd of
    Left err -> err
    Right (cmd, args') -> executeCommand cmd args'

parseCmd :: [String] -> Either (IO ()) (Command, [String])
parseCmd [] = Left $ reportError "No command provided"
parseCmd (x:xs) =
  case getCmd x of
    Nothing -> Left $ reportError "Faulty command name"
    Just cmd -> Right (cmd, xs)
    

