# Programming Paradigms notes & exercises
This repository contains notes and exercises for the course Programming Paradigms at Aalborg University.

## Testing exercise solutions
Requires Haskell, GHC and Cabal.
Ensure testing packages are installed by running the following command, if you haven't already:
```bash
cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit
```

You can test the solutions by running the following command:
```bash
sh test.sh <session num>
```
where `<session num>` is the session number you want to test.

Session 3 is the first lecture with tests for the exercise solutions, so 1 or 2 will not work.