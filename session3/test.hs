module Session3Spec where
    
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession3

main :: IO()
main = hspec $ do
    describe "Session3.onlytwo" $ do
        it "returns False when list has 0 elements" $ do
            onlytwo [] `shouldBe` False
        it "returns True when list has 2 elements" $ do
            onlytwo [0, 1] `shouldBe` True
        it "returns False when list has 3 or more elements" $ do
            onlytwo [0..3] `shouldBe` False
            onlytwo [0..10] `shouldBe` False
            
    describe "Session3.alldots" $ do
        it "returns dot products of two lists of tuples" $ do
            alldots [(1, 1), (2, 2)] [(1, 1), (2, 2)] `shouldBe` [2, 4, 4, 8]
        it "returns dot products of two lists of tuples" $ do
            alldots [(4, 1), (2, 7)] [(5, 3), (1, 4)] `shouldBe` [23, 8, 31, 30]
            
    describe "Session3.pythagoreanTriple" $ do
        it "returns [(3, 4, 5)] with k = 5" $ do
            pythagoreanTriple 5 `shouldBe` [(3, 4, 5)]
        it "returns a list containing correct pythagorean triples" $ do
            pythagoreanTriple 41 `shouldContain` [ (3, 4, 5) ]
            pythagoreanTriple 41 `shouldContain` [ (5, 12, 13) ]
            pythagoreanTriple 41 `shouldContain` [ (7, 24, 25) ]
            pythagoreanTriple 41 `shouldContain` [ (8, 15, 17) ]
            pythagoreanTriple 41 `shouldContain` [ (9, 40, 41) ]
    
    describe "Session3.sevens" $ do
        it "returns a list of ints divisible by 7" $ do
            sevens 5 `shouldBe` [0]
            sevens 21 `shouldBe` [0, 7, 14]
        it "returns a list of ints divisible by 7 where all are less than input" $ do
            let k = 41
            maximum ( sevens k ) < k `shouldBe` True

    describe "Session3.headsup" $ do
        it "returns True if the first two elements in a list are equivalent, otherwise False" $ do
            headsup [1,1,2] `shouldBe` True
            headsup ['a', 'a', 'b'] `shouldBe` True
            headsup ["str", "str", "string"] `shouldBe` True
            headsup [1,2,3] `shouldBe` False

    describe "Session3.flop" $ do
        it "passes example from exercise description" $ do
            flop [(1,'a'),(3,'r'),(9,'e')] `shouldBe` [('a',1),('r',3),('e',9)]
        it "allows for empty list, returning empty list" $ do
            length(flop []) `shouldBe` 0

    describe "Session3.dupli" $ do
        it "passes example from exercise description" $ do
            dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]

    describe "Session3.isperfect" $ do
        it "returns True when given 28" $ do
            isperfect 28 `shouldBe` True
        it "returns False when given a non-perfect number" $ do
            isperfect 117 `shouldBe` False

    describe "Session3.bighead" $ do
        it "returns 2 when passed [7, 4, 5, 8, 9]" $ do
            bighead [7, 4, 5, 8, 9] `shouldBe` 2

    describe "Session3.sums" $ do
        it "returns [2, 3, 3, 4] when passed 2 2" $ do
            sums 2 2 `shouldBe` [2, 3, 3, 4]