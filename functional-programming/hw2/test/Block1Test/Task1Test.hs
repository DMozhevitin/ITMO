module Block1Test.Task1Test
  ( test
  ) where

import           Block1.Task1
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Task 1 test" $ do
    describe "stringSum test" $ do
      it "Empty string sum" $ stringSum "" `shouldBe` Just 0
      it "All words are numbers" $ stringSum "1 2 10" `shouldBe` Just 13
      it "All words aren't numbers" $ stringSum "a b c" `shouldBe` Nothing
      it "One word isn't number" $ stringSum "1 2 a" `shouldBe` Nothing
      it "One word is float" $ stringSum "1 2 22.8" `shouldBe` Nothing
