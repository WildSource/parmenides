# Parmenides
A simple and minimalist SQL migration system written in Haskell.

# Features
It has 4 fundamental features and nothing else.

- An ordered list of migrations
- A tracking table that records which migration ran and those who did not run yet
- A runner that finds unapplied migration and runs them in order
- A way to invoke it (cli binary executable)

# Install
 Parmenides is a single executable binary.  
 You can compile it if you want by cloning the project and running `cabal build`.  
 For most of the time, you can use the pre-compiled binary in the release section of the github repository.

# Usage

Put the executable at the root directory of your project and run the `parmenides setup` command.  
It will create a migrations directory where it will keep and manage sql script files.  
It will also create a environment file (.env) where you have to define some values.  
The variables will already be defined, so all is left to do is to give them values.  



# TODO

- [x] Ordered list of migrations
- [x] A tracking table that records which migration ran and those who did no run yet
- [x] A way to invoke it (cli binary executable)
- [ ] A runner that finds unapplied migration and runs them in order

