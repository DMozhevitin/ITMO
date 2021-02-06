module Block1Test.Task2Test
  ( test
  ) where

import           Block1.Task2     (Nat (..))
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

one = S Z
two = S (S Z)
three = S (S (S Z))
four = S (S (S (S Z)))
eight = S (S (S (S (S (S (S (S Z)))))))
twelve = S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))

test :: IO TestTree
test =
  testSpec "Natural numbers" $ do
    describe "(+) Test" $ do
      -- unit tests
      it "2 + 2 = 4" $ two + two `shouldBe` four
      it "0 + 0 = 0" $ Z + Z `shouldBe` Z
      it "2 + 2 + 8 = 12" $ two + two + eight `shouldBe` twelve

      -- property-based tests
      -- laws from https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Num.html
      it "associativity law" $ (one + one) + one `shouldBe` one + (one + one)
      it "commutativity law" $ one + two `shouldBe` two + one
      it "'fromInteger' 0 is the additive identity" $ two + fromInteger 0 `shouldBe` two

    describe "(*) test" $ do
      -- unit tests
      it "2 * 2 = 4" $ two * two `shouldBe` four
      it "2 * 0 = 0" $ two * Z `shouldBe` Z
      it "2 * 4 = 8" $ two * four `shouldBe` eight

      -- property-based tests
      it "associativity law" $ (one * two) * three `shouldBe` one * (two * three)
      it "fromInteger 1 is the multiplicative identity" $ four * (fromInteger 1) `shouldBe` four
      it "distributivity law" $ one * (two + three) `shouldBe` one * two + one * three

    describe "(-) test" $ do
      it "2 - 2 = 0" $ two - two `shouldBe` Z
      it "4 - 2 = 2" $ four - two `shouldBe` two
      it "0 - 0 = 0" $ Z - Z `shouldBe` Z
      it "0 - 8 = 0" $ Z - eight `shouldBe` Z

    describe "fromInteger test" $ do
      it "fromInteger 0 is Z" $ fromInteger 0 `shouldBe` Z
      it "fromInteger 1 is (S Z)" $ fromInteger 1 `shouldBe` one
      it "fromInteger 2 is (S (S Z))" $ fromInteger 2 `shouldBe` two
      it "fromInteger 4 is (S (S (S (S Z))))" $ fromInteger 4 `shouldBe` four

    describe "toInteger test" $ do
      it "toInteger Z is 0" $ toInteger Z `shouldBe` 0
      it "toInteger Z is 1" $ toInteger one `shouldBe` 1
      it "toInteger Z is 2" $ toInteger two `shouldBe` 2
      it "toInteger Z is 3" $ toInteger three `shouldBe` 3

    describe "(==) and (/=) test" $ do
      -- unit tests
      it "2 + 2 == 3 + 1" $ (two + two) == (three + one) `shouldBe` True
      it "1 + 1 /= 1" $ one + one /= one `shouldBe` True

      -- property-based tests
      it "reflexivity of (==)" $ one == one `shouldBe` True
      it "symmetry of (==)" $ (one == two) == (two == one) `shouldBe` True
      it "negation" $ (one /= one) == not (one == one) `shouldBe` True
      it "substituvity" $ (((+1) (one + one)) == ((+1) (two + Z))) `shouldBe` True

    describe "div test" $ do
      it "4 div 2 = 2" $ four `div` two `shouldBe` two
      it "2 div 4 = 0" $ two `div` four `shouldBe` Z
      it "4 div 4 = 1" $ four `div` four `shouldBe` one

    describe "mod test" $ do
      it "4 mod 2 = 0" $ four `mod` two `shouldBe` Z
      it "2 mod 4 = 2" $ two `mod` four `shouldBe` two
      it "4 mod 4 = 0" $ four `mod` four `shouldBe` Z
      it "(x div y) * y + (x mod y) == x" $ ((S four) `div` two) * two + ((S four) `mod` two) `shouldBe` (S four)

