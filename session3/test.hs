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
            
    -- describe "Session3.pythagoreanTriple" $ do
    --     it "returns (3, 4, 5) with k = 5" $ do
    --         pythagoreanTriple 5 `shouldBe` (3, 4, 5)