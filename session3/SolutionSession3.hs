module SolutionSession3 (
    onlytwo,
    alldots,
    pythagoreanTriple
) where

-- Define, using pattern matching and without using the length function, a function onlytwo that tells
-- us if a list has precisely two elements – in which case it must return True – or not, in which case it
-- must return False. What is the type of onlytwo?
onlytwo :: [x] -> Bool
onlytwo [] = False -- Zero elements
onlytwo [x] = False -- One element
onlytwo [x, y] = True -- Two elements
onlytwo _ = False -- Anything else


-- The dot product of two pairs of numbers (a, b) and (c, d) is the number a · c + b · d. Define, using
-- list comprehension, a function alldots that takes two lists of pairs of numbers and returns all the
-- possible dot products of every pair from the first list and every pair from the second list. Find two
-- good test case for testing your function definition and use them to test your code. What is the type
-- of alldots?
alldots :: Num a => [(a, a)] -> [(a, a)] -> [a]
alldots as bs = [a * c + b * d | 
    (a, b) <- as,
    (c, d) <- bs
    ]



pythagoreanTriple :: Num a => a -> (a, a, a)
pythagoreanTriple n = (n, n, n)
