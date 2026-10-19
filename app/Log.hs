module Log where

import System.Console.ANSI
import System.Console.ANSI.Types (Color (..))

applyColor :: Color -> String -> IO ()
applyColor color str 
  =  setSGR [SetColor Foreground Dull color]
  >> putStr str
  >> setSGR [Reset]
  
logInfo :: String -> IO ()
logInfo msg
  =  setSGR [SetColor Foreground Dull Green]
  >> putStrLn msg
  >> setSGR [Reset]

logWarning :: String -> IO ()
logWarning msg
  =  setSGR [SetColor Foreground Dull Yellow]
  >> putStrLn ("  " ++ msg)
  >> setSGR [Reset]

logError :: String -> IO ()
logError msg
  =  setSGR [SetColor Foreground Dull Red]
  >> putStrLn ("    " ++ msg)
  >> setSGR [Reset]

logCreate :: FilePath -> IO ()
logCreate filepath
  =  putStr "    "
  >> applyColor Green "create"
  >> putStrLn (" " ++ filepath)

logRun :: FilePath -> IO ()
logRun filepath
  =  applyColor Green "    ran "
  >> putStrLn ("migration " ++ filepath)

