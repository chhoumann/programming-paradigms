-- Problem 1
{-
Someone wrote this function:
allAnswers f [] = Just []
allAnswers f (x : xs) =
    let fun = f x
    in  if (fun == Nothing) then Nothing else x : (allAnswers f xs)
    where x = x

Goal:
Apply fn to every element of a list and return a list of the function values obtained.
If one value = Nothing, then the result should be nothing.
-}
-- 1.1 Explain why it doesn't work
{-
There's a typeerror! The first case implies it's a Maybe list return type, but the second case says (else x : ...) which implies a list type.
And also, how would we get x by evaluating x? It wouldn't even terminate.
-}

-- 1.2 Fix the fn with higher order functions
-- Apply fn to every element of a list and return a list of the function values obtained.
-- If one value = Nothing, then the result should be nothing.
allAnswers' f [] = Just []
allAnswers' f xs = if all (/= Nothing) results then Just results else Nothing
    where results = map f xs

-- 1.3 Fix the fn with the Maybe monad
allAnswers'' f []       = Just []
allAnswers'' f (x : xs) = do
    z <- allAnswers'' f xs
    v <- f x
    return (v : z)


-- Problem 2
{-
Best to check this type of exercise with :t in ghci without defining the type. Then you avoid making things too general.
-}
-- 2.1a
foo :: Eq a1 => a2 -> (a1, a1) -> [a2]
foo x (y, z) = [x]
-- foo x (y, z) = if y == z then [x] else []

-- 2.1b
bar :: Eq a => (a -> a -> Bool) -> Bool -> a -> a -> Bool
bar f a b c = True
-- bar f x y z = (f (head [y, z]) z) && x && y == z

-- 2.1c
baz :: Show a => a -> IO b -> IO b
baz a b = b
-- baz x y = do
--  putStr (show x) -- IO Str type
--  z <- getLine
--  y

-- 2.1d
bravo :: (a -> b) -> a -> a -> [b]
bravo f x y = [f x, f y]
-- bravo f x y = [f (head [x, y])]

{-
2.2
a: overloaded & parametric: parametric for a2 and overloaded for a1
b: overloaded & paremetric / WRONG: Only ad hoc (overloaded) because of the class constraint. That's also the only 'type' variable, so it's just overloaded. `a` doesn't count for both.
c: overloaded & parametric: parametric for b and overloaded for a
d: paremetric
-}

-- Problem 3
{-
A binary leaf-labelled a-tree is a binary tree whose leaves are labelled with elements that have type `a`.
-}

-- 3.1
data Tree a = Branch (Tree a) (Tree a) | Leaf a

tree :: Tree String
tree = Branch (Leaf "dog") (Branch (Leaf "cat") (Leaf "hamster"))

-- 3.2
minimax :: Ord a => Tree a -> (a, a)
minimax (Leaf x      ) = (x, x)
minimax (Branch t1 t2) = (min_tree, max_tree)
  where
    (x1, y1) = minimax t1
    (x2, y2) = minimax t2
    min_tree = min x1 x2
    max_tree = max y1 y2


-- Problem 4
-- 4.1
echo = do
    putStr "Please type a word: "
    s <- getLine
    putStrLn ("You typed " ++ s)

echo' =
    putStr "Please type a word: "
        >>  getLine
        >>= (\s -> putStrLn ("You typed " ++ s))

-- 4.2
seconds = do
    x <- getLine
    let blist = read x :: [(Bool, Bool)]
        w = map (\(x,y) -> y) blist in
        do putStr (show w)


-- Problem 5
{-
5.1

It's not allowed in Haskell because lists are homogenous. That is, only values of the same type are allowed in the same lists.
-}

-- 5.2
data Alternating a b = None | Alter a (Alternating b a) deriving Show

-- [5,True,6,False,7,True]:
myAlt = Alter 5 (Alter True (Alter 6 (Alter False (Alter 7 (Alter True None)))))

-- 5.3
separate :: Alternating a b -> ([a], [b])
separate None = ([], [])
separate (Alter x None) = ([x], [])
separate (Alter x (Alter y z)) = (x:xs, y:ys)
    where
        (xs, ys) = separate z

-- 5.4
large n = Alter n (Alter nas (large (n+1)))
    where
        nas = replicate n 'a'

inflist = large 1

-- Problem 6
newtype ToPairs a = TP (a,a)

-- 6.1
alpha :: ToPairs Bool
alpha = TP (True, True)

delta :: ToPairs (Maybe Int -> Int)
delta = TP (x,x)
    where
        x = \(Just x) -> x

-- 6.2
instance Functor ToPairs where
    fmap f (TP (x, _)) = TP (f x, f x)

instance Applicative ToPairs where
    pure x = TP (x, x)
    (TP (f,g)) <*> (TP (x,y)) = TP (f x, g y)