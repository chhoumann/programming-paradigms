module Session4Spec where

import Test.Hspec
import Test.QuickCheck
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

    describe "Session4.reverse'" $ do
        it "reverses [1,2,3] to be [3,2,1]" $ do
            reverse' [1,2,3] `shouldBe` [3,2,1]

    describe "Session4.mylast" $ do
        it "returns the last element in a list" $ do
            mylast [1,2,3,4] `shouldBe` 4

    describe "Session4.wrapup" $ do
        it "returns correct lists given example input" $ do
            wrapup [1,1,1,1] `shouldBe` [[1,1,1,1]]
            wrapup [1,1,1,2,3,3,2] `shouldBe` [[1,1,1],[2],[3,3],[2]]
            wrapup [True,True,False,False,False ,True] `shouldBe` [[ True,True ],[False ,False ,False ],[ True]]

    describe "Session4.rle" $ do
        it "returns correct output given example input" $ do
            rle ['a','a','a','g','g','b','a','a'] `shouldBe` [('a', 3) ,('g', 2) ,('b', 1), ('a', 2)]
            rle [1,1,1,2,2,1,3,3]`shouldBe` [(1,3) ,(2,2) ,(1,1) ,(3,2) ]

    describe "Session4.triples" $ do
        it "returns correct output given example input" $ do
            triples [(1,2,3) , (4, 5, 6), (7, 8, 9)] `shouldBe` ( [1,4,7], [2, 5, 8], [3, 6, 9] )

    describe "Session4.isolate" $ do
        it "returns correct output given example input" $ do
            isolate [4,5,4,6,7,4] 4 `shouldBe` ([5,6,7],[4,4,4])
            isolate ['g','a','k','a'] 'a' `shouldBe` (['g','k'], ['a','a'])

    describe "Session4.amy" $ do
        it "returns correct output given example input" $ do
            let odd x = ( x `mod` 2 ) == 1
            amy odd [ 2 , 5 , 8 , 3 , 7 , 4 ] `shouldBe` True
            amy odd [ 2 , 8 , 42 ] `shouldBe` False
            amy even [ 2 , 3 , 5 ] `shouldBe` True

    describe "Session4.frequencies" $ do
        it "returns correct output given example input" $ do
            frequencies "regninger" `shouldBe`[('r',2),('e',2),('g',2),('n',2),('i',1)]