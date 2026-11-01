module Dialect.Internal where

type Filename = String
type Filepath = String
type Version = String

data Flavor
  = SQLite
  | MySQL
  | PostgreSQL
  deriving Show

mkFlavor :: String -> Maybe Flavor
mkFlavor str =
  case str of
    "sqlite"     -> Just SQLite
    "mysql"      -> Just MySQL
    "postgresql" -> Just PostgreSQL
    _ -> Nothing

{-
Migration (the haskell type) are never created for INSERTs.
They are only created and used when reading from the db.
which means that they are already valid and thus
there's no need for a smart constructor to inforce invariance.
-}
data Migration = Migration
  { identifier :: Int
  , filename :: String
  , filepath :: String  
  , version :: Int
  , applied :: Bool
  } deriving Show

