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

Both the action of applying the migration to the database and
changing the 'applied' field of the table tracking the migration
happens in applyMigrations. This requirement is not enforced by the type system but is extremely important.
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

withSomeDialect
  :: SomeDialect
  -> (forall i c e. Exception e => Dialect i c e -> IO ())
  -> IO ()
withSomeDialect (SomeDialect dialect) f = f dialect

