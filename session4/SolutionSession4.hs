module SolutionSession4(
    replicate',
    improve,
    reverse',
    mylast,
    wrapup,
    rle,
    triples,
    isolate,
    amy,
    frequencies,
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

-- Session exercises
-- Exercise 1
-- The function reverse appears in the Haskell prelude. It will reverse a list such that e.g. reverse
-- [1,2,3] evaluates to [3,2,1] .
-- Now it is your task to define your own version of this function, rev. First try to find out what the
-- type of rev should be and follow the overall approach described in Section 6.6.
reverse' :: Ord a => [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

-- Exercise 2
-- In an earlier session we used other functions from the prelude to define the function last that finds
-- the last element of a list.
-- Now it is your task to give a recursive definition of mylast (which you should call it, since last
-- exists in the prelude). First try to find out what the type of mylast should be and follow the overall
-- approach described in Section 6.6.
mylast :: Ord a => [a] -> a
mylast [] = error "empty list"
mylast [a] = a
mylast (_:xs) = mylast xs

-- Exercise 3
-- The function wrapup is a function that takes a list and returns a list of lists. Each list in this list
-- contains the successive elements from the original list that are identical.
-- For instance, wrapup [1,1,1,2,3,3,2] should give us the list [[1,1,1],[2],[3,3],[2]] and
-- wrapup [True,True,False,False,False ,True] should give us the list [[ True,True ],[False ,False ,False
-- ],[ True]].
-- Define wrapup in Haskell. First try to find out what the type of wrapup should be and follow the
-- overall approach described in Section 6.6
wrapup :: Ord a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:xs) = (x : ts) : wrapup (drop (length ts) xs) -- uses cons operator
    where
        ts = takeWhile (== x) xs -- operator sectioning to define curried function, takes all values equivalent to x (head of argument list)


-- Exercise 4
-- The function rle is a function that, when given a list xs produces a list of pairs of elements of xs
-- and integers. This list of pairs has its elements appears in the order that they appeared originally
-- and contains (x, n) if there are n successive occurrences of x in the list. For instance
-- r l e [ ’ a ’ , ’ a ’ , ’ a ’ , ’ g ’ , ’ g ’ , ’ b ’ , ’ a ’ , ’ a ’ ]
-- should give us the list [(’ a ’,3) ,(’ g ’,2) ,(’ b’,1) ,(’ a ’,2) ] and
-- r l e [ 1 , 1 , 1 , 2 , 2 , 1 , 3 , 3 ]
-- should give us [(1,3) ,(2,2) ,(1,1) ,(3,2) ].
-- Define rle in Haskell. First try to find out what the type of rle should be and follow the overall
-- approach described in Section 6.6.
rle :: Ord a => [a] -> [(a, Int)]
rle [] = []
rle [x] = [(x, 1)]
rle (x:xs) = (x, l+1) : rle (drop l xs)
    where
        l = length (takeWhile (== x) xs)

-- Exercise 5
-- A former minister of science and education now wants to get a masters degree and is learning
-- Haskell. The minister is trying to construct a function triples that takes a list of tuples (each tuple
-- has exactly 3 elements) and converts that list of tuples into a tuple of lists.
-- triples [(1,2,3) , (4, 5, 6), (7, 8, 9)] should produce ( [1,4,7], [2, 5, 8], [3, 6, 9] ).
-- The minister wrote the following piece of code and a type specification but ran into problems. What
-- seems to be wrong?
-- t r i p l e s : : Num a => [ ( a , a , a ) ] −> ( [ a ] , [ a ] , [ a ] )
-- t r i p l e s [ ] = ( )
-- t r i p l e s [ ( a , b , c ) ]= ( [ a ] , [ b ] , [ c ] )
-- t r i p l e s ( x : xs , y : ys , z : z s ) = [ x , y , z ] : T r i p l e s [ ( xs , ys , z s ) ]
-- Can you fix these issues? How can Section 6.6 help you here?
triples :: Num a => [(a,a,a)] -> ([a],[a],[a])
triples vs = (xs, ys, zs)
    where
        xs = [x | (x, _, _) <- vs]
        ys = [y | (_, y, _) <- vs]
        zs = [z | (_, _, z) <- vs]

-- Not sure I'm understanding this one correctly, though.

-- Supplementary exercises
-- Exercise a
-- The function isolate takes a list l and an element x and returns a pair of two new lists (l1 , l2). The
-- first list l1 is a list that contains all elements in l that are not equal to x. The second list l2 is a list
-- that contains all occurrences of x in l.
isolate :: (Eq a) => [a] -> a -> ([a], [a])
isolate xs e = (isNotIn, isIn)
    where
        isIn = [x | x <- xs, x == e]
        isNotIn = [x | x <- xs, x /= e]

-- Exercise b
-- Define a function amy that will tell us if any elements of a list satisfy a given predicate
-- ... just like 'any'
amy :: Eq a => (a -> Bool) -> [a] -> Bool
amy p xs = or (map p xs)

-- Exercise c
-- Create a function frequencies that, given a string s, creates a list of pairs [( x1,f1) ,....( xk,fk)] such
-- that if the character xi occurs a total number of fi times throughout the list s, then the list of pairs
-- will contain the pair (xi, fi ).
frequencies :: [Char] -> [(Char, Int)]
frequencies [] = []
frequencies (x:xs) = (x, length [y | y <- xs, y == x] + 1) : frequencies [y | y <- xs, y /= x]

-- Exercise d
-- A theorem in number theory states that every non-zero real number x can be written as a continued
-- fraction. This is a potentially infinite expression of the form (shown illustration)...
-- For rational numbers, the a_i's will eventually all be 0, so the continued fraction is finite; for irrational
-- numbers, the continued fraction will be infinite. See e.g. [1] for more.
-- The goal of this problem is to write a Haskell function cfrac that will, given a real number r and a
-- natural number n, finds the list of the first n numbers in the continued fraction expansion of r. What
-- should the type of cfrac be?

-- Out of time & not sure how to proceed.