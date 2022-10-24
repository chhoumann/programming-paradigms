module SolutionSession6 where

-- Preparatory exercises

-- Exercise 1
-- Unary numerals consist of a finite sequence of I’s followed by a Z. The natural number n
-- can be represented as n successive I’s and a Z, so e.g. 4 is represented in unary notation
-- as IIIIZ. The natural number 0 is represented as Z.
-- Define a recursive datatype Unary for unary numerals and use your type definition to
-- define a function unary2int of type unary2int :: Unary −> Integer that finds the natural
-- number represented by a given number. As an example, unary IIIIZ should give us 4
data Unary = I Unary | Z

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

-- Exercise 2
-- Use the declaration of the type Tree on page 97 to define a function least that finds the
-- least element in a given binary tree.
-- What should the type of least be?
data Tree a = Leaf a | Empty | Node (Tree a) a (Tree a) 

least :: Tree Int -> Int
least t = minimum (flatten t)

-- Helper
flatten :: Tree t -> [t]
flatten Empty = []
flatten (Leaf n) = [n]
flatten (Node x y z) = flatten x ++ [y] ++ flatten z


-- Primary Exercises

{- 
Exercise 1
Define a Haskell datatype Aexp for arithmetic expressions with addition, multiplication, numerals
and variables. The formation rules are
E ::= n | x | E_1 + E_2 | E_1*E_2
Assume that variables x are strings and that numerals n are integers.
-}
data Aexp = N Int | X [Char] | Add Aexp Aexp | Mult Aexp Aexp


{-
Exercise 2
Use your Haskell datatype from the previous problem to define a function eval that can, when
given a term of type Aexp and an assignment ass of variables to numbers compute the value of the
expression. Hint: Use association lists as described on page 93 to represent assignments.
As an example, if we have the assignment [x 7→ 3, y 7→ 4], eval should tell us that the value of
2 · x + y is 10.
-}
-- Helpers
type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k']

-- Solution
eval :: Aexp -> Assoc [Char] Int -> Int
eval (N i) _ = i
eval (X v) xs = find v xs
eval (Add x y) xs = eval x xs + eval y xs
eval (Mult x y) xs = eval x xs * eval y xs

{- 
Exercise 3
A Unix directory contains other directories and also files. Every directory has a name and a finite
list of directories, which are the subdirectories (there may be no subdirectories at all). Every file
has a name, which is a string, and a size, which is a whole number.
A famous YouTuber was asked to define an algebraic datatype Dir that describes this and came up
with the following.
data Dir a String b Int = Empty Null | Mult Dir Dir | Subdir Dir
The YouTuber remarked that declarations of data types in Haskell do not allow one to specify that
a directory could have any number of subdirectories, so one should therefore assume that there
were always two.
Unfortunately there were problems with the solution. Find out what is wrong and come up with
a better solution. It is a good idea to read the problem text that describes Unix directories very
carefully – and once you have criticized the existing solution, it is also a good idea not to try to
repair what the YouTuber proposed but to start from scratch.
 -}
-- First, the YTs answer leads to an error, so it doesn't even work.
-- From what I can tell, the issue is that they try to type annotate before the assignment, e.g. `String b Int`.
-- And yes, you can specify that a directory can have any number of directories. Just use a list.

-- Helper
type File = (String, Int)
--             Name,  Files, Subdirectories
data Dir = Dir String [File] [Dir]
-- Can't do recursive types, so recursive data declarations it is
-- Example:
home = Dir "~/" [(".config.yml", 57)] [Dir "neovim" [("init.lua", 17)] []]

{- 
Exercise 4
On page 98, the book describes search trees; make sure that you understand what important
property a search tree has.
Assume that our type for trees is defined as
data Tree a = Leaf a | Empty | Node ( Tree a ) a ( Tree a )
This means that trees can now also be empty. Define a function
insert :: Ord a => Tree a −> a −> Tree a
such that whenever t is a search tree, then insert t x gives us a new search tree that now also
contains x.
-}
-- Search tree: If you apply a 'flatten' function (like the one I made earlier)
-- and the list you get back is sorted, then the tree is a search tree.
-- Likewise, if you are trying to determine whether a given value occurs in a tree, you can
-- determine which of the two subtrees in which it may be easily. If the value is less than the value at the node, 
-- it may only occurs in the left subtree. Otherwise, the right subtree.
insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf l) x = if x > l then Node (Leaf l) x Empty else Node (Leaf x) l Empty
insert (Node ll v rl) x = if x <= v then Node (insert ll x) v rl else Node ll v (insert rl x)

{- 
Exercise 5
We say that a binary tree is balanced if the number of leaves in every left and right subtree differ
by at most one with leaves themselves being trivially balanced. Define a function balanced that
will tell us if a binary tree is balanced or not. Hint: It is a good idea to also define a function that
finds the number of leaves of a tree.
-}
isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Leaf _) = True
isBalanced (Node l _ r) = abs (countLeaves l - countLeaves r) <= 1 && isBalanced l && isBalanced r

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Leaf _) = 1
countLeaves (Node l _ r) = countLeaves l + countLeaves r