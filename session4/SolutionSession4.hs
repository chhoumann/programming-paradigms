module SolutionSession4(
    replicate',
    improve
) where

-- Preparatory exercises
-- Exercise 1
-- Define the function replicate – and use pattern matching in your solution. This function takes an
-- integer n and and an element x and gives us a list with n elements where x has been repeated
-- exactly n times. As an example, replicate 3 5 should give us [5,5,5]. What should the type of
-- replicate be?

-- simple solution: replicate' n x = take n (repeat x)
replicate' :: Int -> x -> [x]
replicate' 0 _ = [] -- the below case actually covers this but I had to use pattern matching
replicate' n x = take n (repeat x) -- this is the same as just using Prelude.replicate

-- Exercise 2
-- Define the function improve – and use pattern matching in your solution. It takes a list xs and, if
-- xs contains at least two elements, it gives us a list where every other element has been removed.
-- As an example, improve [1,2,3,4,5,6,7] should give us [1,3,5,7]. What should the type of improve
-- be?
improve :: [a] -> [a]
improve xs = [x | (x, i) <- zip xs [0..], even i]

