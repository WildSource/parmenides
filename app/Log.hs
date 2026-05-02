module Log where

import System.Console.ANSI
import System.Console.ANSI.Types (Color (..))

applyColor :: Color -> String -> IO ()
applyColor color str 
  =  setSGR [SetColor Foreground Vivid color]
  >> putStr str
  >> setSGR [Reset]
  
logInfo :: String -> IO ()
logInfo msg
  =  setSGR [SetColor Foreground Vivid Green]
  >> putStrLn msg
  >> setSGR [Reset]

logWarning :: String -> IO ()
logWarning msg
  =  setSGR [SetColor Foreground Vivid Yellow]
  >> putStrLn ("  " ++ msg)
  >> setSGR [Reset]

logError :: String -> IO ()
logError msg
  =  setSGR [SetColor Foreground Vivid Red]
  >> putStrLn ("    " ++ msg)
  >> setSGR [Reset]

