module Block1Test.Task2Test
  ( test
  ) where

import           Block1.Task2
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

singleLeaf :: Tree Int
singleLeaf = Leaf 1

simpleTree :: Tree Int
simpleTree = Branch (Leaf 1) (Leaf 2)

fLeaf :: Tree (Int -> Int)
fLeaf = Leaf (*10)

simpleFTree :: Tree (Int -> Int)
simpleFTree = Branch fLeaf (Leaf (*100))

test :: IO TestTree
test =
  testSpec "Task 2 test" $ do
    describe "instance Functor Tree test" $ do
      it "fmap on single leaf test" $ (+1) <$> singleLeaf `shouldBe` Leaf 2
      it "fmap on simple tree test" $ (+1) . (*2) <$> simpleTree `shouldBe` Branch (Leaf 3) (Leaf 5)

    describe "instance Applicative Tree test" $ do
      it "Leaf <*> Leaf test" $ fLeaf <*> singleLeaf `shouldBe` Leaf 10
      it "Branch <*> Leaf test" $ simpleFTree <*> singleLeaf `shouldBe` Branch (Leaf 10) (Leaf 100)
      it "Leaf <*> Branch test" $ fLeaf <*> simpleTree `shouldBe` Branch (Leaf 10) (Leaf 20)
      it "Branch <*> Branch test" $ simpleFTree <*> simpleTree `shouldBe` Branch (Leaf 10) (Leaf 200)
      
    describe "instance Foldable Tree test" $ do
      it "sum Leaf test" $ sum singleLeaf `shouldBe` 1
      it "sum Branch test" $ sum simpleTree `shouldBe` 3
      it "foldr on function Leaf test" $ foldr (.) (+1) fLeaf 1 `shouldBe` 20
      it "foldr on function Branch test" $ foldr (.) (+2) simpleFTree 1 `shouldBe` 3000

