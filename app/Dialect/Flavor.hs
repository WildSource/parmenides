{-|
Module: Dialect.Flavor
Description: Contains the definition of a Flavor
Copyright: (c) WildSource, 2023
License: MIT
Maintainer: ilovetrap123@hotmail.com

This module defines what a Dialect Flavor is and what it can do.
A flavor is a term used to describe a database. As in, is it mysql ? Is it postgresql ?
A flavor is just a Sum Type and is constructed using the mkFlavor function.
-}
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
    
