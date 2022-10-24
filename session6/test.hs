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

    describe "Session6.eval" $ do
        it "evaluates expression given expression and variable assignments" $ do
            let xs = [("x", 3), ("y", 18), ("z", 93)]

            let exp1 = Add (N 1) (N 2)
            eval exp1 xs `shouldBe` 3

            let exp2 = Mult (X "x") (N 3)
            eval exp2 xs `shouldBe` 9

            let exp3 = (X "y" `Add` (X "z" `Mult` N 29)) `Mult` N 22
            eval exp3 xs `shouldBe` 59730

            let xs2 = [("x", 3), ("y", 4)]
            let exp4 = N 2 `Mult` X "x" `Add` X "y"
            eval exp4 xs2 `shouldBe` 10

    describe "Session6.insert" $ do
        it "returns a search tree after insertion in a search tree" $ do
            let st = Node (Leaf 1) 2 (Leaf 3)
            let ft = Node (Node (Leaf 1) 2 Empty) 2 (Leaf 3)
            
            -- Just making sure
            flatten st `shouldBe` [1,2,3]
            flatten ft `shouldBe` [1,2,2,3]

            flatten (insert st 2) `shouldBe` flatten ft

            flatten (insert (insert (insert (insert Empty 1) 2) 18) 13) `shouldBe` [1, 2, 13, 18]

    describe "Session6.isBalanced" $ do
        it "Determines that a balanced binary tree is balanced" $ do
            {- 
                3
               / \
              2   4
             /
            1
            -}
            let bt1 = Node (Node (Leaf 1) 2 Empty) 3 (Node Empty 4 Empty)
            isBalanced bt1 `shouldBe` True

        it "Determines that an unbalanced binary tree is unbalanced" $ do
            {- 
                    4
                   / \
                  3   2
                 / \
                2   3 
               / \
              2   1
            -}

            let bt2 = Node (Node (Node (Leaf 2) 2 (Leaf 1)) 3 (Leaf 3)) 4 (Node Empty 2 Empty)
            isBalanced bt2 `shouldBe` False