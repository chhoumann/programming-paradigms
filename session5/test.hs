module Session5Spec where
    
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession5

main :: IO()
main = hspec $ do
    describe "Session5.positions" $ do
        it "returns correct output given example input" $ do
            positions "abba" `shouldBe` [1,2,2,1]

    describe "Session5.sumsq" $ do
        it "returns correct output given example input" $ do
            sumsq 4 `shouldBe` 30
            sumsq 9 `shouldBe` 285

    describe "Session5.within" $ do
        it "returns correct output given example input" $ do
            SolutionSession5.within [1,3,4,5,2] (1,3) `shouldBe` [1,3,2]
            SolutionSession5.within [1,3,4,5,2] (3,1) `shouldBe` []

    describe "Session5.sumrows" $ do
        it "returns correct output given example input" $ do
            sumrows [[1,2], [3,4]] `shouldBe` [3, 7]
            sumrows [[],[],[1]] `shouldBe` [0, 0, 1]

    describe "Session5.approx" $ do
        it "approximates natural exponential fraction e" $ do
            approx 100 `shouldSatisfy` (\x -> x >= 2.7 && x <= 2.8) -- floating points are hard to test, especially approximationso
    
    describe "Session5.fingo" $ do
        it "appends list to another list" $ do
            fingo [1,2,3,4] [5,6] `shouldBe` [5,6,1,2,3,4]

    describe "Session5.foltr" $ do
        it "filters list elements given predicate" $ do
            let tls1 = [1,2,3,4,5]
            foltr even tls1 `shouldBe` filter even tls1
            foltr odd tls1 `shouldNotBe` filter even tls1

    describe "Session5.remove" $ do
        it "removes all chars from second passed string that is in first passed string" $ do
            remove "first" "second" `shouldBe` "econd"