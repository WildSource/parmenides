# Parmenides
![Watercolour painting of Parmenides](docs/parmenides_watercolor.png "Watercolour of Parmenides by yours truly") 
A simple and minimalist SQL migration system written in Haskell.

> Parmenides was a pre-socratic philosopher known for monism which asserts that reality is a single, unchanging whole and that change and motion are illusions.

# Features and Goals
It has 4 fundamental features and nothing else.__
The goal is to be simple (and easy but it's not the priority) and out of your way.__

> For the distinction of "simple" and "easy" refer to [this](https://www.youtube.com/watch?v=SxdOUGdseq4) 

With the design being minimal and simple some safety "features" are not present.__
Depending on the safety "feature" (like checksum) some, will be implemented later but for most, they are the developer's responsability to not do stupid things.__
Migrations are forward only, this means that if you mess up, you make another migrations to fix your old one.


- An ordered list of migrations
- A tracking table that records which migration ran and those who did not run yet
- A runner that finds unapplied migration and runs them in order
- A way to invoke it (cli binary executable)

# Status
It's ready for 1.0 but not stable yet.  
Meaning that the api is definitive for 1.0 but reliability and program behavior are not yet stable.__

> Although the happy path has been ran (it works) but not thoroughly tested yet.
> That's why reporting issues will be important.

## Stability
- Every migrations are ran through a transaction that rollback if there's an error.
- Paremnides has idempotency, meaning that if you rerun a migration it will not change the db or anything.
- No migration checksum yet.

# Install
 Parmenides is a single executable binary.  
 I use Linux so if you are on Mac or Windows you compile it yourself.
 
## From source
 Clone the project and build it by calling this command `cabal build`.  
 Or just run it with `cabal run` with also runs the build command implicitly.__
 
## Releases
 There's a pre-compiled binary in the release section of the github repository for Linux users.  
 You can also compile it even if you are on Linux.  

# Usage
The help command displays a list of commands in order of execution in a workflow.

> Except for the findMigration command which is called implicitly by migrate but is exposed for the user to use if creating migrations by hand.
> "By hand" I mean not using the mkMigration command (which you should).

```
Usage: parmenides CMD [ARG ...]
Commands:
  help                This is the help command
  setup               Creates a migrations/ directory and an environment file for parmenides
  setupTracking       Create a table in the database to track migrations
  mkMigration TITLE   Creates a migration file to migrations/ and tracks it
  findMigrations      Checks for new migrations that are not tracked and add entries for them
  migrate             Runs findMigrations first cmd and then runs unapplied migrations
```

## Setup 
1. Put the executable at the root directory of your project  
2. Run the `parmenides setup` command.  
   It will create a migrations directory where it will keep and manage sql script files.  
   It will also create a environment file (parmenides.env) where you have to define some values.  
   The variables will already be defined, so all is left to do is to give them values.  
   **Very important**, the parser for env files needs a newline at the end of the file or it will give you an error.  
3. With the values to the env file provided, run the `parmenides setupTracking` command to create a tracking table in your current database.__
   This table is your single source of truth for managing which migrations have been ran. (it does not store the sql script that's what the filesystem is for)__
   
## Workflow

4. Run the `parmenides mkMigration TITLE` command.  
>  Replace *TITLE* by any name. They have to be connected tho using `"CREATE FRUIT TABLE"` will not work and throw an error.  
>  Rather use `CREATE_FRUIT_TABLE` since the underscore connect these words to form only one.  
>  Capitalized letters are used in the examples but any alphanumeric is accepted.  
   It will create a migration file in *migrations/* and add a database entry so that parmenides can track it.  
   The migration file is an ordinary sql file so all of the ordinary database and sql rule applies.  
   A migration's filename is in the form `TITLE_YYYYMMDDHHmmss.sql`.  
   Parmenides will not enforce the filename's format, so if you mess it up you will have to fix it (for now).
   This means that if you have **any** type of file with **any** name Parmenides **will** track it as a migration file and throw an error when trying to run it.  
   In that case, you go into the database and delete the faulty one (they are ordered anyways).  
   
5. Run the `parmenides migrate` command.  
   Which implicitly executes the findMigrations commands to find any migrations that are missing or not tracked and adds them to the tracking table (Usually migrations that are added manually and not through the mkMigration command)  
   Then gets all the migrations that are not applied and applies them sequentially in an ordered manner. Each migrations are ran through a transaction so if any returns an error it will stop and report the error.
   
> Checksums are not implemented yet   
   
# Roadmap

Error message are kind of lack luster but they will be implemented in the future when specific errors will arise from issue reporting (Hopefully).

## For 0.1.0.1
- [ ] Migration checksum

## For 0.2.0.0
- [ ] Sqlite support

