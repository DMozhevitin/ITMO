module Block3Test.Task2Test
  ( test
  ) where

import           Block3.Task2     (Endo (..), Name (..), NonEmpty (..),
                                   ThisOrThat (..))
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)


toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

getName :: Name -> String
getName (Name n) = n

endo1 :: Endo String
endo1 = Endo (\s -> s ++ "AAA")

endo2 :: Endo String
endo2 = Endo (\s -> s ++ "BBB")

endo3 :: Endo String
endo3 = Endo (\s -> s ++ "228")

test :: IO TestTree
test =
  testSpec "semigroups" $ do
    describe "NonEmpty test" $ do
        it "associativity test" $ toList (((1 :| []) <> (2 :| [])) <> (3 :| [])) `shouldBe` toList ((1 :| []) <> ((2 :| []) <> (3 :| [])))
    describe "ThisOrThat test" $ do
        it "This <> This" $ (This [1] :: ThisOrThat [Int] [Int]) <> (This [2] :: ThisOrThat [Int] [Int]) `shouldBe` This [1, 2]
        it "This <> That" $ (This [1] :: ThisOrThat [Int] [Int]) <> (That [2] :: ThisOrThat [Int] [Int]) `shouldBe` Both [1] [2]
        it "This <> Both" $ (This [1] :: ThisOrThat [Int] [Int]) <> (Both [2] [3] :: ThisOrThat [Int] [Int]) `shouldBe` Both [1, 2] [3]
        it "That <> That" $ (That [1] :: ThisOrThat [Int] [Int]) <> (That [2] :: ThisOrThat [Int] [Int]) `shouldBe` That [1, 2]
        it "That <> This" $ (That [1] :: ThisOrThat [Int] [Int]) <> (This [2] :: ThisOrThat [Int] [Int]) `shouldBe` Both [2] [1]
        it "That <> Both" $ (This [1] :: ThisOrThat [Int] [Int]) <> (Both [2] [3] :: ThisOrThat [Int] [Int]) `shouldBe` Both [1, 2] [3]
        it "Both <> This" $ (Both [1] [2] :: ThisOrThat [Int] [Int]) <> (This [3] :: ThisOrThat [Int] [Int]) `shouldBe` Both [1, 3] [2]
        it "Both <> That" $ (Both [1] [2] :: ThisOrThat [Int] [Int]) <> (That [3] :: ThisOrThat [Int] [Int]) `shouldBe` Both [1] [2, 3]
        it "Both <> Both" $ (Both [1] [2] :: ThisOrThat [Int] [Int]) <> (Both [3] [4] :: ThisOrThat [Int] [Int]) `shouldBe` Both [1, 3] [2, 4]
    describe "Name test" $ do
        it "associativity test" $ getName ((Name "a" <> Name "b") <> (Name "c")) `shouldBe` getName (Name "a" <> (Name "b" <> (Name "c")))
        it "mempty test 1" $ getName (Name "a" <> mempty) `shouldBe` "a"
        it "mempty test 2" $ getName (mempty <> (Name "a")) `shouldBe` "a"
    describe "Endo test" $ do
        it "associativity test" $ (getEndo ((endo1 <> endo2) <> endo3)) "" `shouldBe` (getEndo (endo1 <> (endo2 <> endo3))) ""
        it "mempty test 1" $ (getEndo (mempty <> endo1)) "" `shouldBe` (getEndo endo1) ""
        it "mempty test 1" $ (getEndo (endo1 <> mempty)) "" `shouldBe` (getEndo endo1) ""
