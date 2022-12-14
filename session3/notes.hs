abs :: Int -> Int
abs n = if n >= 0 then n else -n
-- can nest conditionals.
-- need to have else branch.

-- Guarded Equations
abs1 n  | n >= 0 = n
        | otherwise = -n

signum n    | n <- 0 = -1
            | n == 0 = 0
            | otherwise = 1 -- otherwise defined to be = True


-- Pattern Matching
-- Patterns are matched in order, from top to bottom.
-- Patterns may not repeat variables. E.g. b && b = b is not allowed.
not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False -- means 'anything and anything = false'. Underscore is wildcard symbol basically. Replaces:
-- True && False = False
-- False && True = False
-- False && False = False

-- But in Haskell it's actually defined like this:
-- True && b = b
-- False && _ = False
-- Because it's more efficient due to the lazy evaluation mechanism.
-- It avoid evaluating the second argument if the first one is false.

-- List Patterns
head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

-- Lambda Functions
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- Function that returns the list of all positions of a value in a list
-- Works by generating pairs between the lists xs and [0..], where x' is the value from xs and i is the index, generated by [0..]
-- and then it guards by checking that x == x', so we only get those indices where x' is the number we specified
positions :: Eq a => a -> [a] -> [Int]
positions x xs =
    [i | (x', i) <- zip xs [0..], x == x']


-- Using zip, you can define a function that returns the list of all pairs of adjacent elements from a list
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (Prelude.tail xs)


-- And we can use this to define a function that decides if the elements in a list are sorted
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]