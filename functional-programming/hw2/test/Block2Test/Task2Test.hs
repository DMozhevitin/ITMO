module Block2Test.Task2Test
  ( test
  ) where

import           Block2.Task2
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
import           Data.Either (isLeft)

test :: IO TestTree
test =
  testSpec "Task 2 test" $ do
    describe "simple moving averate test" $ do
      it "sample test #1" $ moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5] 
      it "sample test #2" $ moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
      it "single element window test" $ moving 1 [1, 2, 3, 4, 5] `shouldBe` [1.0, 2.0, 3.0, 4.0, 5.0]
      it "max size window test" $ moving 5 [1, 2, 3, 4, 5] `shouldBe` [1.0, 1.5, 2.0, 2.5, 3.0]
      it "constant function test #1" $ moving 1 [1, 1, 1, 1] `shouldBe` [1.0, 1.0, 1.0, 1.0]
      it "constant function test #2" $ moving 2 [1, 1, 1, 1] `shouldBe` [1.0, 1.0, 1.0, 1.0]
      it "constant function test #3" $ moving 3 [1, 1, 1, 1] `shouldBe` [1.0, 1.0, 1.0, 1.0]
      it "constant function test #4" $ moving 4 [1, 1, 1, 1] `shouldBe` [1.0, 1.0, 1.0, 1.0]