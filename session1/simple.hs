-- This is the simple program from the slides from the introduction

laengde :: (Num p) => [a] -> p

laengde [] = 0
laengde (x:l) = 1 + (laengde l)

myList = [2,3,17,9,69,484000]
myCharList = ['a', 'b', 'c', 'd', 'e', 'f']

data BTree = BLeaf Int | BBranch Int BTree BTree deriving Show

-- sumtree :: BTree -> Int

sumtree (BLeaf x) = x
sumtree (BBranch x t1 t2) = let v1 = sumtree t1
                                v2 = sumtree t2
                            in x + v1 + v2


myBigOak = BBranch 14 (BLeaf 13) (BLeaf 17)


-- Quicksort

-- For any type a of ordered values, map between lists of such values.
-- So it can support different types of (ordered) lists
qsort :: (Ord a) => [a] -> [a]

qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
                 where small = qsort [a | a <- xs, a <= x] -- sort a list of elements from xs that are <= than x
                       big   = qsort [a | a <- xs, a > x] -- sort a list of elements from xs that are > x

-- where introduces local definitions.
-- 'small' and 'big' are used with notation similar to set builder.