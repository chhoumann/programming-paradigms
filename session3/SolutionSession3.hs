module SolutionSession3 (
    onlytwo,
    alldots,
    pythagoreanTriple,
    sevens,
    headsup,
    flop,
    dupli,
    isperfect,
    bighead,
    sums
) where

-- Prep exercise 1
-- Define, using pattern matching and without using the length function, a function onlytwo that tells
-- us if a list has precisely two elements – in which case it must return True – or not, in which case it
-- must return False. What is the type of onlytwo?
onlytwo :: [x] -> Bool
onlytwo [] = False -- Zero elements
onlytwo [x] = False -- One element
onlytwo [x, y] = True -- Two elements
onlytwo _ = False -- Anything else


-- Prep exercise 2
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


-- Exercise 1
-- A Pythagorean triple is a triple (a, b, c) of natural numbers a, b, and c, such that a ≤ b < c and
-- a^2 + b^2 = c^2. In other words, a triple of this form gives us the length of the three sides of a right
-- triangle for which all sides have integer length. The smallest Pythagorean triple is (3, 4, 5).
-- Use list comprehension to define a function pyt that, when given an integer k, gives us a list of all
-- Pythagorean triples whose largest element is at most k. Before you write the definition of pyt, find
-- out what its type should be.
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n = [(a, b, c) |
    c <- [1..n],
    b <- [0..c-1],
    a <- [0..b],
    a <= b,
    b < c,
    a^2 + b^2 == c^2
    ]

-- Exercise 2
-- Use list comprehension to define a function sevens that given an integer k gives us a list of all
-- natural numbers that are divisible by 7 and are less than k. First find out what its type should be.
sevens :: Int -> [Int]
sevens k = [x | x <- [0..k], x `mod` 7 == 0, x < k]


-- Exercise 3
-- During breaks in the recording of the reality TV show Paradise Hotel a contestant decided to write
-- a function headsup that can tell us if the two first elements of a list are identical. Here is what the
-- contestant wrote.
-- headsup x = if head x == head (tail x) then True else False
-- The contestant added that the type of headsup is
-- [Num] −> Bool
-- The other contestants argued that this solution was clumsy and that there seemed to be something
-- very wrong with the type. What is your opinion and why?

-- First, I think you should just discard the conditionals.
-- Second, you need to make sure that the list items are comparable, hence we use Eq type class below.
-- The contestant only allowed for numbers, but the same function could work for other list types.
headsup :: Eq a => [a] -> Bool
headsup x = head x == head (tail x)

-- Exercise 4
-- Show how the meaning of the following curried function definition can be given in terms of lambda
-- expressions from Haskell.
-- plonk x y z = x + y + z
plonk :: Num a => a -> a -> a -> a
plonk x = (\y -> (\z -> x+y+z))

-- Exercise 5
-- Find a Haskell expression whose type is:
ex5 :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
ex5 c1 c2 (xs, hs) = if c1 /= c2 then xs else hs


-- Supplementary exercises
-- Exercise a
-- Use list comprehension to define a function flop that, when given a list of pairs returns a list of pairs
-- whose components are reversed. The list can be empty.
-- For example, flop [(1,’a’),(3,’r’),(9,’e’)] should return the list [(’a’,1),(’r’,3),(’e’,9)].
-- What is the type of flop?
flop :: [(a, b)] -> [(b, a)]
flop ls = [(b, a) | (a, b) <- ls]

-- Exercise b
-- Write a function dupli that will duplicate the elements of any given list. As an example, dupli [1, 2, 3]
-- must give us [1,1,2,2,3,3]. What should the type of dupli be? Hint: The concat function from Chapter
-- 5 will be useful for stitching everything together.
dupli ls = concat [[x, x] | x <- ls]

-- Exercise c
-- A perfect number n is a natural number that is the sum of its own divisors that are not n. 28 is a
-- perfect number, since 1 + 2 + 4 + 7 + 14 = 28. Use list comprehension to define a function isperfect
-- that will tell us if any given natural number is a perfect number
isperfect :: Int -> Bool
isperfect n = sum divisors == n
    where
        divisors = [x | x <- [1..n], n `mod` x == 0, x < n]

-- Exercise d
-- Last week, we read that a famous influencer on Instagram has defined a Haskell function bighead that
-- can tell us how many elements in a list xs are greater than (>) the head of xs. As an example of the
-- behaviour of the function instance, the result of bighead [7,4,5,8,9] will be 2.
-- Now it is your turn to be a famous influencer. How would you define the bighead function? What
-- should its type be?
bighead :: Ord a => [a] -> Int
bighead [] = 0
bighead (x:xs) = length [x' | x' <- xs, x' > x]

-- Exercise e
-- Here is a function sums whose definition has one single use of list comprehension.
-- sums m n = [ x+y | x <− [1..m], y <− [1..n] ]
-- The list comprehension in this definition uses two generators. Write an alternative definition of sums
-- that only uses list comprehensions (so you may need more than one instance of list comprehension)
-- with one generator each. Hint: The concat function from Chapter 5 will also be useful here.

-- I take this assignment to ask me to replace the two generators with list comprehensions, each of which
-- can have a single generator. I think I am misunderstanding, though.
sums m n = [x + y | 
    x <- [x | x <- [1..m]],
    y <- [x | x <- [1..n]]
    ]