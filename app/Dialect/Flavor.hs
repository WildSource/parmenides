module Dialect.Flavor where

data Flavor
  = SQLite 
  | MySQL 
  | PostgreSQL

mkFlavor :: String -> Maybe Flavor
mkFlavor str =
  case str of
    "sqlite"     -> Just SQLite
    "mysql"      -> Just MySQL
    "postgresql" -> Just PostgreSQL
    _ -> Nothing
    
