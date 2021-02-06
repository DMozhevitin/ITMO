module Block2Test.Task1Test
  ( test
  ) where

import           Block2.Task1
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
import           Data.Either (isLeft)

test :: IO TestTree
test =
  testSpec "Task 1 test" $ do
    describe "eval test" $ do
      it "2 + 2 = 4" $ eval (Add (Const 2) (Const 2)) `shouldBe` Right 4  
      it "2 + 2 * 2 = 6" $ eval (Add (Mul (Const 2) (Const 2)) (Const 2)) `shouldBe` Right 6
      it "1 / 0 is DivisionByZero" $ eval (Div (Const 1) (Const 0)) `shouldBe` Left DivisionByZero
      it "0 ^ (-1) is NegativePower" $ eval (Pow (Const 0) (Const (-1))) `shouldBe` Left NegativePow
      it "(0 ^ (-1)) / 0 should be any error" $ isLeft (eval (Div (Pow (Const 0) (Const (-1))) (Const 0))) `shouldBe` True
      it "1 / (1 - 1) is still DivisionByZero" $ eval (Div (Const 1) (Sub (Const 1) (Const 1))) `shouldBe` Left DivisionByZero