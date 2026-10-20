module Dialect.Internal where

data ParmenidesMigration = ParmMigration
  { id :: Int
  , filepath :: String
  , applied :: Bool
  } deriving Show

