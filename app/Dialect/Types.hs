{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-} 
{-|
Module: Dialect.Types
Description: Contains various data about Dialects
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module defines some type aliases and the definition of a Migration.
Migrations are data representation of the table instances in a database.
When parmenides fetches data from the database the rows get converted into a Migration.
-}
module Dialect.Types
  ( Migration (..)
  , MigrationMatch (..)
  , Filename
  , Filepath
  , Version
  ) where

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

{-
This type is just to enforce the structure of the regex match
when matching the migration filenames.
It's used in the migrate command.
-}
data MigrationMatch = MigrationMatch
  { filepath :: String
  , filename :: String
  , timestamp :: String
  } deriving Show

instance Eq MigrationMatch where
  match == match' =
    let filepath = match.filepath
        filepath' = match'.filepath
    in  filepath == filepath'

instance Ord MigrationMatch where
  (MigrationMatch _ _ tstmp) <= (MigrationMatch _ _ tstmp') =
    let numTstmp = (read tstmp :: Integer) 
        numTstmp' = (read tstmp' :: Integer) 
    in  numTstmp <= numTstmp'

