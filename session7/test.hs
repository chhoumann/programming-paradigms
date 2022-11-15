module Session7Spec where
    
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession7

main :: IO()
main = hspec $ do
    describe "Session7.triples" $ do
        it "handles exercise test case" $ do
            triples [(1,2,3), (4,5,6), (7,8,9)] `shouldBe` ([1,4,7], [2,5,8], [3,6,9])

