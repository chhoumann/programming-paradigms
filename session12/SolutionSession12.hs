module SolutionSession12 where

{-
PREPARATORY EXERCISES - EXERCISE 1
Give two different definitions, one recursive and one not, of a function `nsonly` that takes as input a number n and gives us the infinite list consisting of 0n, 1n, 2n, 3n,...
-}
nsonlyr :: Int -> [Int]
nsonlyr n = nsonlyr' n 0
    where
        nsonlyr' n x = x * n : nsonlyr' n (x+1) -- recall: `:` is the cons operator, which means `x*n` is being added to the start of the list, `nsonlyr' n (x+1)`.

nsonly :: Int -> [Int]
nsonly n = [n * x| x <- [0..n+1]]

{-
PREPARATORY EXERCISES - EXERCISE 2
We can define the following:
x = 1 : (map (1+) x)
and then evaluate take 5 x.

One might think that in fact the following happens:
take 5 x
= 1:2:map (+1) x
= 1:2:map (+1) [1, 2]
= 1:2:2:3:map (+1) x
= 1:2:2:3:map (+1) [1,2,2,3]
= 1:2:2:3:2:3:3:4:map (+1) x

Explain why this is wrong.
"That is because the Haskell interpreter gives a different result" is not a valid answer.
You have to provide an evaluation sequence as the ones presented in the text for today.
-}
{-
Haskell uses lazy evaluation, which means that it's call by name + sharing of arguments through pointers.
Given that, the above explanation is wrong.
take 5 x
= 1:map (+1) [1]
= 1:2:map (+1) [1:2] -- has computed 1, so doesn't do that again
= 1:2:3:map (+1) [1:2:3] -- and so on...
= 1:2:3:4:map (+1) [1:2:3:4]
= [1:2:3:4:5]

_I am very unsure whether this is correct or not._
-}

{-
PROBLEM SET - EXERCISE 1
In Haskell, the value undefined has type `a`. One can put it anywhere and it will compile. But if one tries to evaluate it, it throws the exception "undefined".
Here is a function called `indflet`:
-}
indflet :: a -> [a] -> [a]
indflet _ [] = []
indflet _ [x] = [x]
indflet e (x:y:ys) = x : e : indflet e (y:ys)
{-
First try to figure out what its type is and what the function does (without asking the Haskell interpreter).
Then, again without asking, figure out why an exception is thrown when you evaluate
head (indflet 1 (2:undefined))
-}
{-
First: the function inserts a number at every other element in a list. Type is written above the fn.
Second: An exception is thrown because the function is trying to work with 'undefined' when it can't.
This happens in the second pattern match case, which is the same as `indflet _ (x:[]) = ...`.
It tries to evaluate whether undefined == [], which throwns the exception.

Below is a fixed version.
-}

{-
PROBLEM SET - EXERCISE 2
Fix `indflet` in a function `fletind` which doesn't throw when you evalute the previously mentioned expression.
-}
fletind :: a -> [a] -> [a]
fletind _ [] = []
fletind e (x:xs) = x : e : fletind e xs
{- Fixed by not using the second case and ditching the unnecessary 'y/ys'. If xs == [] it just matches the first case. -}

{-
PROBLEM SET - EXERCISE 3
Define a function allBinaries :: [String] that gives the infinite ordered list of all binary numbers, with the least significant bit first, no trailing zeros, i.e.
allBinaries = ["0", "1", "01", "11", "001", ...]
-}
allBinaries :: [String]
allBinaries = "0" : "1" : [x ++ y | x <- allBinaries, y <- ["0", "1"]]
-- Intuitively, this is wrong. It isn't an ordered list by any means. 
-- But apparently, it gets the same output as the professor's approach, so either he wrote the function or the exercise wrong.

{-
PROBLEM SET - EXERCISE 4
Trees can be defined by
-}
data Tree = Node Tree Tree | Leaf
data Direction = L | R --- for left/right
type Path = [Direction]
{-
Define a function, allFinitePaths :: Tree -> [Path], that takes a binary tree t :: Tree (can be infinite) and gives a list of all finite paths from the root to any leaf of t.
-}
t :: Tree
t = Node (Node Leaf Leaf) Leaf

allFinitePaths :: Tree -> [Path]
allFinitePaths t = go t []
    where go Leaf path = [path]
          go (Node l r) path = go l (path ++ [L]) ++ go r (path ++ [R])


{-
PROBLEM SET - EXERCISE 5 (optional)
Copied this solution from Pat's notes for reference.

A problem, due to mathematician W. R. Hamming, is to write a program that produces an infinite list of natural numbers with the following properties:
1. The list is in ascending order, without duplicates
2. The list begins with the number 1
3. If the list contains the number x, then it also contains the numbers 2x, 3x, and 5x
4. The list contains no other numbers

Define a function `hamming` that gives such a list.
-}
hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))
    where merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) (y:ys)
              | x < y = x : merge xs (y:ys)
              | x > y = y : merge (x:xs) ys
              | otherwise = x : merge xs ys