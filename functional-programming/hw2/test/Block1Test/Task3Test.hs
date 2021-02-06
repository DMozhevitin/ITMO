module Block1Test.Task3Test
  ( test
  ) where

import           Block1.Task3 (NonEmpty(..))
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Task 3 test" $ do
    describe "instance Functor NonEmpty test" $ do
      it "fmap on single element test" $ (+1) <$> 1 :| [] `shouldBe` 2 :| []
      it "fmap test #2" $ Just <$> 1 :| [2, 3] `shouldBe` Just 1 :| [Just 2, Just 3]
      it "fmap id test" $ id <$> 1 :| [] `shouldBe` 1 :| []
    
    describe "instance Applicative NonEmpty test" $ do
        it "pure test" $ pure 1 `shouldBe` 1 :| []
        it "<*> test on NonEmpty with single element" $ ((+1) :| []) <*> pure 1 `shouldBe` pure 2
        it "<*> test on NonEmpty with multiple elements" $ ((+1) :| [(+2)]) <*> (1 :| [2, 3]) `shouldBe` 2 :| [3, 4, 3, 4, 5]

    describe "instance Monad NonEmpty test" $ do
        it "return test" $ return 1 `shouldBe` 1 :| []
        it "single bind test" $ (1 :| [] >>= \x -> return (x + 1)) `shouldBe` 2 :| []
        it "double bind test" $ (1 :| [] >>= \x -> (2 :| []) >>= \y -> (x + y) :| []) `shouldBe` 3 :| []

    describe "instance Foldable NonEmpty test" $ do
        it "sum test" $ sum (1 :| [2, 3]) `shouldBe` 6
        it "product test" $ product (1 :| [2, 3]) `shouldBe` 6
        it "concat test" $ concat ("1" :| ["2", "3"]) `shouldBe` "123"
        it "foldr subtract test" $ foldr (-) 0 (1 :| [2, 3]) `shouldBe` 2    