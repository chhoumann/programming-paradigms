module SolutionSession8 where
import           AsmCodeGen                     ( x86NcgImpl )

-- DISCUSSION PROBLEMS - EXERCISE 1
greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")

-- DISCUSSION PROBLEMS - EXERCISE 2
-- What does `sequence_ [putStr "rip", putStr "rap", return ()]` do?
-- And why will haskell complain about `[putStr "rip", putStr "rap", getChar]`?

{-
1. Haskell complains about the last line because it is not a monadic value.
2. The first line prints "rip" and "rap" and then returns the unit value.
-}

-- PROBLEM SET - EXERCISE 2
letters :: IO ()
letters = do
    x <- getLine
    if x == "\n"
        then return ()
        else do
            letter x
            return ()
  where
    letter []       = return ()
    letter (x : xs) = do
        putStrLn [x]
        letter xs


-- PROBLEM SET - EXERCISE 3
-- Give another definition of `letters` that uses `sequence_` function.
letters' :: IO ()
letters' = do
    x <- getLine
    if x == "\n"
        then return ()
        else do
            letter x
            return ()
  where
    letter []       = return ()
    letter (x : xs) = sequence_ [putStrLn [x], letter xs]

-- PROBLEM SET - EXERCISE 4
{- 
Define an action `hugorm :: IO ()` that reads a give number of integers from the keyboard, one per line, and then finally displays the sum of the integers.
-}
hugorm :: IO ()
hugorm = do
    putStrLn "How many numbers?"
    n <- getLine
    hugorm' (read n :: Int) 0
  where
    hugorm' 0 acc = putStrLn ("The sum is " ++ show acc)
    hugorm' n acc = do
        x <- getLine
        hugorm' (n - 1) (acc + (read x :: Int))