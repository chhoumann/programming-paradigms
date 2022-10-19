module SolutionSession5(
    positions,
    sumsq,
    within,
    sumrows,
    approx,
    fingo
) where

-- Preliminary exercises

-- Exercise 1
-- Every letter in the lowercase English alphabet has a position. ”a” has position 1, ”c” has
-- position 3 and ”h” has position 8.
-- In Haskell, every string is a list of characters. So String is the same type as [Char].
-- We can define a function positions that, given a string of lowercase letters str gives us the
-- list of positions of the characters in str.
-- As an example, positions ”abba’’ gives us [1,2,2,1] . Use the higher-order functions in
-- Chapter 7 to define positions.
-- Here it useful to remember that the ordinal value of a character can be computed using
-- the function fromEnum found in the prelude. We have that fromEnum ’a’ is 97 and that
-- fromEnum ’b’ is 98.
positions :: [Char] -> [Int]
positions = map (\x -> fromEnum x - 96) -- see above explanation for why 96

-- Exercise 2
-- The function sumsq takes an integer n as its argument and returns the sum of the squares
-- of the first n integers. So sumsq n returns the sum
-- 1 + . . . + n^2
-- As an example, sumsq 4 gives us 30 and sumsq 9 gives us 285 . Use foldr to define sumsq
-- – and do not use map.
--sumsq :: Int -> Int
sumsq n = foldr (\x y -> x^2 + y) 0 [0..n]


-- Primary exercises

-- Exercise 1
-- The within function takes a list of numbers and a pair of numbers, returns a list of numbers which
-- are in the input list and within the range (inclusive) given by the input pair.
-- The elements in the output list appear be in the same order they appeared in the input list. If the
-- input pair is (n1,n2), assume that n1 is the lower bound of the range and n2 is the upper bound of
-- the range.
-- As an example, within [1,3,4,5,2] (1,3) should give us [1,3,2] and within [1,3,4,5,2] (3,1) should
-- give us [] .
-- Define within using the higher-order functions in Chapter 7.
within :: (Ord a, Eq a) => [a] -> (a, a) -> [a]
within xs (min, max) = filter (\x -> x >= min && x <= max) xs


-- Exercise 2
-- Implement the sumrows function. The function takes a list of number lists returns a one-dimensional
-- list of numbers with each number equal to the sum of the corresponding row in the input list. If a
-- list is empty, its sum is 0.
-- As an example, sumrows [[1,2], [3,4]] should give us [3, 7], and sumrows [[],[],[1]] should give
-- us [0,0,1] .
-- Define sumrows using the higher-order functions in Chapter 7.
sumrows :: Num a => [[a]] -> [a]
sumrows = map sum

-- Exercise 3
-- The base of the natural exponential function e = 2.718 . . . can be written as the limit of the infinite
-- series \Sigma^{\infty}_{k=0}\frac{1/k!}
-- The function approx should give us the approximation of e that we find by adding the first n terms
-- of this infinite series. That is, approx n = \Sigma^{\infty}_{k=0}\frac{1/k!}
-- Define approx using the higher-order functions in Chapter 7; the factorial function k! is defined by
-- fact k = product [1..k]
approx n = sum (map (\x -> 1/product [1..x]) [0..n])

-- Exercise 4
-- Here is a function. What does it do? And why? Use the explanation at the very end of Section 7.3
-- in the book to help you answer the ”why”-question.
fingo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys

-- Looks like it appends a list to another list.
-- Why it does it sounds like more of a philosophical question.
-- But I'll take it asks me for the explanation of how it does what it does.
-- When called, you are actually writing
-- fingo (:) [1,2,3] (4:(5:[]))
-- which expands to
-- (4:(5:[1,2,3]))
-- which evaluates to
-- [4,5,1,2,3]