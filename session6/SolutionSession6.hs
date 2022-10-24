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
data Tree a = Leaf a | Node (Tree a) a (Tree a)

least :: Tree Int -> Int
least t = minimum (get_tree_vals t)
    where
        get_tree_vals :: Tree t -> [t]
        get_tree_vals (Leaf n) = [n]
        get_tree_vals (Node x y z) = get_tree_vals x ++ [y] ++ get_tree_vals z


-- Primary Exercises

