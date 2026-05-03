module Main (main) where

import System.Environment (getArgs)
import LoadEnv (loadEnv)

main :: IO ()
main = do
  loadEnv

  args <- getArgs

  print args
  
