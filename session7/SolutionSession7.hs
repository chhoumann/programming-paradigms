module SolutionSession7 where

-- Exercise 1
{-
I solved this exercise earlier. Here's a better solution.
-}
triples :: [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples ((a, b, c):xs) = (a:as, b:bs, c:cs)
    where (as, bs, cs) = triples xs

{-
This problem set is a 'greatest hits' of the course so far. Opting to not do the exercise again right now.
-}