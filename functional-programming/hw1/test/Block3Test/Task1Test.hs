module Block3Test.Task1Test
  ( test
  ) where

import           Block3.Task1
import           Data.List.NonEmpty
import           Test.Tasty         (TestTree)
import           Test.Tasty.Hspec   (describe, it, shouldBe, testSpec)


test :: IO TestTree
test =
  testSpec "monoids" $ do
    describe "maybeConcat test" $ do
        it "sample test" $ maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1, 2, 3, 4, 5]
        it "maybebaby concat test" $ maybeConcat [Just "abc", Just "def"] `shouldBe` "abcdef"
    describe "eitherConcat test" $ do
        it "lists test" $ eitherConcat [Left [1], Right [2], Left [3], Right [4]] `shouldBe` ([1, 3], [2, 4])
        it "maybes test" $ eitherConcat [Left (Just [1]), Right (Just [2])] `shouldBe` (Just [1], Just [2])
        it "maybe nothings test" $ eitherConcat [Left (Just [1]), Right (Just [2]), Left Nothing, Right Nothing] `shouldBe` (Just [1], Just [2])
