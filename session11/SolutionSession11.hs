module SolutionSession11 where
import           Parsing

-- PREPARATORY EXERCISES - EXERCISE 1
data Onion = Core Int | Layer Onion deriving Show

theOnion :: Parser Onion
theOnion =
    do
        char 'L'
        l <- theOnion
        return (Layer l)
    <|> -- If the above fails, do this instead... See 'Making choices' in Parsing.hs
        do
            x <- int
            return (Core x)

{-
There are a few alternatives to this one.
You can do
    do char 'L'
        Layer <$> theOnion

or 
    do char 'L"
        fmap Layer theOnion

Because these are equivalent.
And the same for the other case.
-}

-- PREPARATORY EXERCISES - EXERCISE 2
data Grammar = Multi Char Grammar Char | Empty deriving Show

theGrammar :: Parser Grammar
theGrammar =
    do
        x       <- char 'a'
        grammar <- theGrammar
        y       <- char 'b'
        return (Multi x grammar y)
    <|> do
            return Empty


-- PROBLEM SET - EXERCISE 2
data Rexp = A | B | Union Rexp Rexp | Concat Rexp Rexp | KStar Rexp deriving Show

expr =
    do
            x <- term
            char 'O'
            y <- expr
            return (Concat x y)
        <|> do
                x <- term
                char 'U'
                y <- expr
                return (Union x y)
        <|> term

term =
    do
        char '('
        x <- expr
        char ')'
        char '*'
        return (KStar x)
    <|> factor

factor =
    do
        char 'a'
        return A
    <|> do
            char 'b'
            return B
