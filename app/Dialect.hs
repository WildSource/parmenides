{-|
Module: Dialect
Description: Contains the definition of a database Dialect
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module defines what a database dialect is and what it can do.
-}
{-# LANGUAGE ExistentialQuantification #-}
module Dialect
  ( Dialect (..)
  , SomeDialect (..)
  , withSomeDialect 
  ) where

-- Prelude imports
import Control.Exception (Exception)

-- User code imports
import Dialect.Types
  ( Filename
  , Filepath
  , Version
  , Migration
  )

{-
Dialect is a record that represents the API for database interactions.
Every sql dialect or database support must implement these functions.

The type variable 'i' represents the Connection information type.

The type variable 'c' represents the Connection type that represents a connection to the database.

The type variable 'e' represents the Error type that execute or query return.

WithConnection and WithTransaction have to be defined by just using the functions of the same name ffrom the "*-simple" libraries. Since the function api and arguments are different for each supported database of the "simple" libraries we have to wrap these functions.

Both the action of applying the migration to the database and
changing the 'applied' field of the table tracking the migration
are represented by applyMigration and markApplied. The should be used
when implementing Command.migrateCmd by sequentially running them in a transaction.
Enforcing this using the Type system is very complicated so just remember to do it if you plan on implementing a database support. Or just go look at how Dialect.Sqlite and Dialect.Postgresql are made.

Every function returns either Data which in that case are helper functions.
Returns Either e () which is an operation that can fail.
When it fails an exception is thrown by the "*-simple" library which is caught and converted to an Either.

Database interactions returns a possible error value (with Either) and Commands handle reporting the error.

These operations should be small and minimal they interact with the database and nothing else.
If something fails they throw an Exception (usually automatically) and Commands handles them.
-}
data Dialect i c e = Exception e => Dialect
  { getConnectionInfo :: IO i
  , withConnection :: i -> (c -> IO ()) -> IO ()
  , withTransaction :: c -> IO () -> IO ()
  , createMigrationTrackingTable :: c -> IO (Either e ())
  , createTrackedMigration
    :: c
    -> Filename
    -> Filepath
    -> Version
    -> IO (Either e ())
  , applyMigration :: c -> Migration -> IO (Either e ())
  , markApplied :: c -> Int -> IO (Either e ())
  , getOrderedUnappliedMigrations :: c -> IO [Migration]
  , getTrackedMigrationNames :: c -> IO [String]
  }

data SomeDialect = forall i c e. Exception e => SomeDialect (Dialect i c e)

{-
Since we want to support multiple databases, we have to have a generic api.
However by being generic, we cannot assume anything about the dialect.
This means no specific properties or exposing actual types used by the dialect.
Which is why the withSomeDialect function exist. It is meant to hide and scope
out the usage of dialect and no value or anything should escape from this function.
You want to do anything within a database ? You do it inside of withSomeDialect.
-}
withSomeDialect
  :: SomeDialect
  -> (forall i c e. Exception e => Dialect i c e -> IO ())
  -> IO ()
withSomeDialect (SomeDialect dialect) f = f dialect

