module Block1Test.Task3Test
  ( test
  ) where

import           Block1.Task3
import           Data.List.NonEmpty
import           Test.Tasty         (TestTree)
import           Test.Tasty.Hspec   (describe, it, shouldBe, testSpec)

testTree = Tree (2 :| []) (Tree (1 :| []) Leaf Leaf) (Tree (3 :| []) Leaf Leaf)

test :: IO TestTree
test =
  testSpec "Binary Search Trees" $ do
    describe "isEmpty test" $ do
      it "single leaf is empty" $ isEmpty Leaf `shouldBe` True
      it "other trees aren't empty" $ isEmpty testTree `shouldBe` False

    describe "size test" $ do
        it "single leaf's size = 0" $ size Leaf `shouldBe` 0
        it "(fromList [1, 2, 3])'s size = 3" $ size testTree `shouldBe` 3

    describe "contains test" $ do
        it "single leaf doesn't contain 1" $ Leaf `contains` 1 `shouldBe` False
        it "single leaf doesn't contain 2" $ Leaf `contains` 2 `shouldBe` False
        it "single leaf doesn't contain 3" $ Leaf `contains` 3 `shouldBe` False

        it "(fromList [1, 2, 3]) contains 1" $ testTree `contains` 1 `shouldBe` True
        it "(fromList [1, 2, 3]) contains 2" $ testTree `contains` 2 `shouldBe` True
        it "(fromList [1, 2, 3]) contains 3" $ testTree `contains` 3 `shouldBe` True
        it "(fromList [1, 2, 3]) doesn't contain 228" $ testTree `contains` 228 `shouldBe` False

    describe "insert test" $ do
        it "(fromList [1, 2, 3]) `insert` 4 contains 4" $ ((testTree `Block1.Task3.insert` 4) `contains` 4) `shouldBe` True
        it "(fromList [1, 2, 3]) `insert` 3 still contains 3" $ ((testTree `Block1.Task3.insert` 3) `contains` 3) `shouldBe` True

    describe "fromList test" $ do
        it "toList . fromList = id (for sorted list)" $ (Block1.Task3.toList (Block1.Task3.fromList [1, 2, 3])) `shouldBe` [1, 2, 3]

    describe "remove test" $ do
        it "(fromList [1, 2, 3]) doesn't contain 3 after removing 3" $ (testTree `remove` 3) `contains` 3 `shouldBe` False
        it "(fromList [1, 2, 3]) after removing 4 is still (fromList [1, 2, 3])" $ (Block1.Task3.toList $ testTree `remove` 4) `shouldBe` [1, 2, 3]
