{-# LANGUAGE ExistentialQuantification #-}
module Dialect where

import Control.Exception (Exception)

import Dialect.Types
  ( Filename
  , Filepath
  , Version
  , Migration
  )

{-
Dialect is a record that represents the API for database interactions.
Every sql dialect or database support must implement these functions.

The type variable 'c' represents the Connection type that represents a connection to the database.

The type variable 'e' represents the Error type that execute or query return.

Both the action of applying the migration to the database and
changing the 'applied' field of the table tracking the migration
happens in applyMigrations. This requirement is not enforced by the type system but is extremely important.
-}
data Dialect c e = Exception e => Dialect
  { getConnection :: IO c
  , createMigrationTrackingTable :: c -> IO (Either e ())
  , createTrackedMigration
    :: c
    -> Filename
    -> Filepath
    -> Version
    -> IO (Either e ())
  , applyMigration :: c -> Migration -> IO (Either e ())
  }

data SomeDialect = forall c e. Exception e => SomeDialect (Dialect c e)
    
withSomeDialect
  :: SomeDialect
  -> (forall c e. Exception e => (Dialect c e -> IO ()))
  -> IO ()
withSomeDialect (SomeDialect dialect) f = f dialect

