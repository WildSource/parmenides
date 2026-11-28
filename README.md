# Parmenides
![Watercolour painting of Parmenides](docs/parmenides_watercolor.png "Watercolour of Parmenides by yours truly")  

A simple and minimalist SQL migration system written in Haskell.

> Parmenides was a pre-socratic philosopher known for monism which asserts that reality is a single, unchanging whole and that change and motion are illusions.

# Features and Goals
It has 4 fundamental features and nothing else.  
The goal is to be simple (and easy but it's not the priority) and out of your way.  

> For the distinction of "simple" and "easy" refer to [this](https://www.youtube.com/watch?v=SxdOUGdseq4) 

With the design being minimal and simple some safety "features" are not present.  
Depending on the safety "feature" (like checksum) some, will be implemented later but for most, they are the developer's responsability to not do stupid things.  
Migrations are forward only, this means that if you mess up, you make another migrations to fix your old one.  
**Supports Postgresql and Sqlite (MySQL some day)**  

- An ordered list of migrations
- A tracking table that records which migration ran and those who did not run yet
- A runner that finds unapplied migration and runs them in order
  - Every migrations are ran through a transaction that rollback if there's an error.
  - Parmenides has idempotency, meaning that if you rerun a migration it will not change the db.
- A way to invoke it (cli binary executable)

# Install
 Parmenides is a single executable binary.  
 I use Linux (Desktop) and OpenBSD (Laptop), so if you are on Mac or Windows you compile it yourself.  
 
## From source
 Clone the project and build it by calling this command `cabal build`.  
 Or just run it with `cabal run` with also runs the build command implicitly.  
 
## Releases
 There's a pre-compiled binary in the release section of the repository.  
 The binary will either be for Linux or OpenBSD depending on which devices I'm on when releasing.  
 (Look I'm doing my best ok)  

# Usage
The help command displays a list of commands in order of execution in a workflow.
(Except for readMigrations)

```
Usage: parmenides CMD [ARG ...]
Commands:
  help                This is the help command
  setup DATABASE      Creates a migrations/ dir and a db specific env file
  setupTracking       Create a table in the database to track migrations
  mkMigration TITLE   Creates a migration file to migrations/ and tracks it
  readMigrations      Reads the migrations/ dir and tracks missing migrations
  migrate             Runs unapplied migrations tracked by Parmenides
```

## Setup 
1. Put the executable at the root directory of your project  

2. Run the `parmenides setup DATABASE` command.  
   The `DATABASE` argument takes either "sqlite", "postgresql" or "mysql".  
   Although, avoid "mysql" since it's not supported (my bad on that part).  
   It will create a migrations directory where it will keep and manage sql script files.  
   It will also create a environment file (parmenides.env) where you have to define some values.  
   The variables will already be defined, so all is left to do is to give them values.  
   If a gitignore file is present, `parmenides.env` is automatically added to your gitignore.  
   
   > **Very important**
   > The parser for env files needs a newline at the end of the file or it will give you an error.  
   >
   > The flavor environment variable is what parmenides uses to know which database dialect to use. 
   > It enables you to not need to provide it as an argument in every command everytime you do something.  
   > 
   > All of these quirks are already handled by the `setup DATABASE` command.  

### SQLite 
When using SQLite, you just have to provide the path towards your database file.

```
PARMENIDES_FLAVOR=sqlite
PARMENIDES_DB_FILE=
```

### PostgreSQL
Using PostgreSQL is a little bit more work. Remember that Parmenides is just a migration tracking tool.  
You still need to create a Database and a Role with the appropriate permissions.  
Then provide the host which refers to which machine to connect to so either an ip address or `localhost`.  
Then provide the port which by default is `5432`.  
Then provide the name of the Role (User) that will connect to the database.  
Then provide the name of the Database itself.  
Then provide the password to authenticate yourself.  

```
PARMENIDES_FLAVOR=postgresql
PARMENIDES_HOST=
PARMENIDES_PORT=
PARMENIDES_USER=
PARMENIDES_NAME=
PARMENIDES_PASSWORD=
```
   
3. With the values to the env file provided, run the `parmenides setupTracking` command to create a tracking table in your current database.  
   This table is your single source of truth for managing which migrations have been ran. (it does not store the sql script that's what the filesystem is for)  
   
## Workflow

4. Run the `parmenides mkMigration TITLE` command.  
   It will create a migration file in *migrations/* and add a database entry so that parmenides can track it.  
   The migration file is an ordinary sql file so all of the ordinary database and sql rule applies.  
   A migration's filename is in the form `TITLE_YYYYMMDDHHmmss.sql`.  
   It is very important to respect the filename structure of a migration file.  
   However, as long as you use the `mkMigration` command you are good.  
   Replace *TITLE* by any name. They have to be connected tho using `"CREATE FRUIT TABLE"` will not work and throw an error.  
   Rather use `CREATE_FRUIT_TABLE` since the underscore connect these words to form only one and do not use double quotes.  
   Capitalized letters are used in the examples but any alphanumeric is accepted.  
   
   > Parmenides only tracks migrations created through the `mkMigration TITLE` command.
   > Any file in the *migration/* directory not put there by the command is ignored completely.
   
5. Run the `parmenides migrate` command.  
   Parmenides gets migrations that have not been applied yet and applies them sequentially in an ordered manner.  
   Each migrations are ran through a transaction so if any returns an error it will stop and report the error.  
   
### Read migrations command

This command is not needed when doing solo development or using migrations created by the `mkMigration` command.  
The `readMigrations` command is meant to add third party migrations to the tracking table.   

Meaning that if you are working with other developers and you pull the migrations made by another dev, (meaning not created by *YOUR* `mkMigration` command) you will be able to track and use them.  

This also means that *YOU*, the developer is also able to manually create migrations files without the command (mkMigration). However, that is extremely *NOT* recommended. This feature was previously removed to make sure that only Parmenides could control the creation and format of migration files which avoided bugs or defective migrations (and also less work from the dev itself). The command is only meant to add migrations that comes from places other than your own othewise, collaborative work would be impossible.
	
# Status
Parmenides is stable.  
It does what it's supposed to do and the API is stable.  
Error message are kind of lack luster but they will get better as time goes on and it is used more.  
Hopefully, specific errors get reported to issues.  
	
