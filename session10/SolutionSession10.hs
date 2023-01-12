module SolutionSession10 where

-- PREPARATORY EXERCISES -- Exercise 1
{-
Define a function `tuple :: Monad m => m a -> m b -> m (a, b)` using explicit (>>=) and then do-notation.
What does the function do in the case where the monad is Maybe?
-}
tuple :: Monad m => m a -> m b -> m (a, b)
tuple ma mb = ma >>= (\a -> mb >>= (\b -> return (a, b)))

tuple' :: Monad m => m a -> m b -> m (a, b)
tuple' ma mb = do
  a <- ma
  b <- mb
  return (a, b)

{-
In the case where the monad is Maybe,
the function will take in two Maybe values (ma and mb) and
return a single Maybe value containing a tuple of the values contained within the original Maybe values.
If either of the input Maybe values are Nothing, the output will be Nothing.
Otherwise, the output will be Just (a, b) where a and b are the values 
contained within the input Maybe values.
-}

-- PREPARATORY EXERCISES -- Exercise 2
{-
What is the expression that uses (>>=) equivalent to the following do block?
do y <- z
   s y
   return (f y)
-}
foo :: Monad m => (t -> b) -> m t -> (t -> m a) -> m b
foo f z s = do
    y <- z
    s y
    return (f y)

bar :: Monad m => (t -> b) -> m t -> (t -> m a) -> m b
bar f z s = 
    z >>= \y ->
    s y >> 
    return (f y)
-- Basically a line-by-line translation.
-- >> disregards the output generated by the first action, so we don't need to make (\_ -> ...).

-- PROBLEM SET - EXERCISE 1
fourfirst xs = do
    x <- xs
    return (4, x)
{-
Claim: "This function takes a list and gives us a pair (4,x) where x is the first element of the list".
Is this claim true? Explain what the code does and how.
-}
{-
The claim is not true.
The function binds x to xs.
To understand this, recall the definition of the bind operator for the list monad:
xs >>= f = concat (map f xs)

This is just flatMap.
The bind operator applies f to each element in the list using map,
and then concatenates the resulting lists using concat.

So this is what happens:
-}
fourfirst' xs = xs >>= \x -> return (4,x)
{-
The return function is used to wrap a value in a monad.
In this case, the return function is used to wrap the pair (4, x) in a list monad, resulting in a list of pairs.

This is because it's actually applying the function `return (4, x)`, which wraps the pair (4, x) in a list monad.

So this is what happens:
concat (map (\x -> return (4, x)) [1,2,3])
-}

{- STATE.hs FILE START -}

-- Defining states and state transformers
newtype State = SE [Int] deriving Show

newtype ST a = ST (State -> (a, State))

app :: ST a -> State -> (a,State)

app (ST st) x = st x

-- Defining the ST monad

-- We must first make ST a functor

instance Functor ST where
  -- fmap :: (a->b) -> ST(a->b)
   fmap g st = ST (\s -> let (x,s') = app st s in (g x,s'))

-- Then we must make ST an applicative functor

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = ST (\s -> (x,s))

  -- <*> :: ST (a->b) -> ST a -> ST b

  stf <*> stx = ST (\s ->
                  let (f,s')  = app stf s
                      (x,s'') = app stx s' in (f x,s''))

-- Finally, we can make ST a monad; we only need to define >>= as return is simply the -- pure function

instance Monad ST where
  -- >>= :: a -> (a -> ST b) -> ST b
  st >>= f = ST (\s ->
                let (x,s') = app st s in app (f x) s')

{- STATE.hs FILE END -}

-- PROBLEM SET - EXERCISE 2 & 3
{-
Given the above (except instances), implement stack operations:
- get: state transformer that, given a state s, gives us the top of the stack (if it exists) and 0 otherwise _and_ the same state s
- put: takes integer x as argument and then, given state s, gives us state s' where x has been placked on top of the stack
- remove: takes a state s and gives us a state s' where the top element from the stack of s has been discarded
-}
get :: ST Int
get = ST (\(SE xs) -> case xs of
    [] -> (0, SE xs)
    (x:xs) -> (x, SE xs))

put :: Int -> ST Int
put x = ST (\(SE xs) -> (x, SE (x:xs)))

remove :: ST Int
remove = ST (\(SE xs) -> case xs of
    [] -> error "no"
    (x:xs) -> (x, SE xs))

-- PROBLEM SET - EXERCISE 4
{-
Implement
- push that gives a stack where a new integer is placed on top of the stack
- pop that gives the top element of a stack and a stack from which this top element has been removed
- add that adds the two uppermost values x and y on the stack and gives us a stack with a new top element, x+y, where x and y have been removed
- mult that does the same as above but multiplies instead
-}
push :: Int -> ST Int
-- push x = ST (\(SE xs) -> (x, SE (x:xs)))
push x = do put x

pop :: ST Int
-- pop = ST (\(SE (x:xs)) -> (x, SE xs))
pop = do remove

add :: ST Int
-- add = ST (\(SE (x:(y:ys))) -> (x+y, SE ((x+y):ys)))
add = do
    x <- pop
    y <- pop
    push (x+y)

mult :: ST Int
-- mult = ST (\(SE (x:(y:ys))) -> (x*y, SE ((x*y):ys)))
mult = do
    x <- pop
    y <- pop
    push (x*y)

baz = do 
    push 2
    push 3
    add
    push 5
    mult
    pop

result = app baz (SE [0])

{- Commented out code is 'equivalent' to written code. -}