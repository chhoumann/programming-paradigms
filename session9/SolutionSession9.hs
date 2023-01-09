module SolutionSession9 where
import qualified SolutionSession6 as example

-- PREPARATORY EXERCISES - EXERCISE 1
{-
An onion consists of a finite number of layers surrounding a core. In this problem,
we let the core be value. Below is a declaration of an algebraic datatype Onion a parameterized by the type a.
-}
data Onion a = Core a | Layer (Onion a)
{-
Define Onion as an instance of Functor.
Hint: Find inspuration in book, showing how Tree can become a functor
-}
instance Functor Onion where
    fmap f (Core a) = Core (f a)
    fmap f (Layer o) = Layer (fmap f o)

-- PROBLEM SET - EXERCISE 1
data UTree a = Node a [UTree a]
{-
Define an instance of Functor for UTree
-}
instance Functor UTree where
    fmap f (Node x xs) = Node (f x) (map (fmap f) xs)

-- PROBLEM SET - EXERCISE 2
{-
The function type constructor ((->)r) is defined such that f a will be (r -> a).
Define an instance of Functor for this type constructor.
-}
-- instance Functor ((->) r) where
--   fmap f g = f . g
-- Already defined so leads to error if uncommented


-- PROBLEM SET - EXERCISE 3
-- Can't find example.

-- PROBLEM SET - EXERCISE 4
-- Lambda calculus isn't pensum and I don't care about it.