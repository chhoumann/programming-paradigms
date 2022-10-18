module Session4Spec where
    
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession4

main :: IO()
main = hspec $ do
    describe "Session4.replicate'" $ do
        it "replicate 3 5 returns [5,5,5]" $ do
            replicate' 3 5 `shouldBe` [5,5,5]
        it "replicate 0 'd' returns []" $ do
            replicate' 0 'd' `shouldBe` []
    
    describe "Session4.improve" $ do
        it "returns [1,3,5,7] when given [1,2,3,4,5,6,7]" $ do
            improve [1,2,3,4,5,6,7] `shouldBe` [1,3,5,7]


