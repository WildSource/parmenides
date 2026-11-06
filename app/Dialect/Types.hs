module Dialect.Types where

type Filename = String
type Filepath = String
type Version = String

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

