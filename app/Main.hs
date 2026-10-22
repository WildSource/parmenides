module Main (main) where

import Data.List (find)
import System.Environment (getArgs)
import LoadEnv (loadEnvFrom)

import Command (Command (..))
import Commands
  ( commands
  , errorCommand
  )

main :: IO ()
main = do
  loadEnvFrom "parmenides.env"
  args <- getArgs
  either
    id
    (\ (cmd, arguments) -> run cmd arguments)
    (parse args)
                
parse :: [String] -> Either (IO ()) (Command, [String])
parse [] = Left $ errorCommand "No command has been provided to parmenides"
parse (x : xs) =
  let maybeCommand = find (\ cmd -> name cmd == x) commands
  in
    case maybeCommand of
      Nothing ->
        Left $ errorCommand $ "No command named " ++ x ++ " was found"
      Just matchedCmd ->
        Right (matchedCmd, xs)
