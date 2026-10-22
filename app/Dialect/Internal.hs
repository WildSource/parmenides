module Dialect.Internal where

data Flavor
  = SQLite
  | MySQL
  | PostgreSQL
  deriving Show

getFlavor :: String -> Maybe Flavor
getFlavor str =
  case str of
    "sqlite"     -> Just SQLite
    "mysql"      -> Just MySQL
    "postgresql" -> Just PostgreSQL
    _ -> Nothing

data ParmenidesMigration = ParmMigration
  { id :: Int
  , filepath :: String
  , applied :: Bool
  } deriving Show

