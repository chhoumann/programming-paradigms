module Session6Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession6

main :: IO()
main = hspec $ do
    describe "Session6.unary2int" $ do
        it "from Unary IIIIZ get 4" $ do
            unary2int (I(I(I(I Z)))) `shouldBe` 4

    describe "Session6.least" $ do
        it "gets smallest value in tree" $ do
            {-
                   5
                 /   \
                3     7
               / \    / \
              1   4  6   9
            -}
            let t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
            least t `shouldBe` 1

            {-
                   5
                 /   \
                3     7
               / \    / \
              9   4  6   9
            -}
            let t = Node (Node (Leaf 9) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
            least t `shouldBe` 3

