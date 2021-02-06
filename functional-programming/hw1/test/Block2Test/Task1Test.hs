module Block2Test.Task1Test
  ( test
  ) where

import           Block1.Task3
import           Data.List.NonEmpty
import           Test.Tasty         (TestTree)
import           Test.Tasty.Hspec   (describe, it, shouldBe, testSpec)

testTree = Tree (2 :| []) (Tree (1 :| []) Leaf Leaf) (Tree (3 :| []) Leaf Leaf)

test :: IO TestTree
test =
  testSpec "Foldable tree" $ do
    describe "foldr test" $ do
        it "foldr + (fromList [1, 2, 3]) = 6" $ foldr (+) 0 testTree `shouldBe` 6
        it "foldr * (fromList [1, 2, 3]) = 6" $ foldr (*) 1 testTree `shouldBe` 6
    describe "foldMap test" $ do
        it "foldMap (:) (fromList[1, 2, 3]) = [1, 2, 3]" $ (foldMap (: []) testTree) `shouldBe` [1, 2, 3]

