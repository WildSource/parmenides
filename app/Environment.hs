module Environment where

migrationDir :: String
migrationDir = "./migrations"

migrationUpDir :: String
migrationUpDir = migrationDir ++ "/up"

migrationDownDir :: String
migrationDownDir = migrationDir ++ "/down"

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
    
