module Block1Test.Task1Test
  ( test
  ) where

import           Block1.Task1
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Days of Week" $ do
    describe "nextDay test" $ do
      it "Sunday then Monday" $ nextDay Sunday `shouldBe` Monday
      it "Monday then Tuesday" $ nextDay Monday `shouldBe` Tuesday
      it "Tuesday then Wednesday" $ nextDay Tuesday `shouldBe` Wednesday
      it "Wednesday then Thursday" $ nextDay Wednesday `shouldBe` Thursday
      it "Thursday then Friday" $ nextDay Thursday `shouldBe` Friday
      it "Friday then Saturday" $ nextDay Friday `shouldBe` Saturday
      it "Saturday then Sunday" $ nextDay Saturday `shouldBe` Sunday

    describe "afterDays" $ do
      it "Monday after Sunday 1 day" $ afterDays Sunday 1 `shouldBe` Monday
      it "Sunday after Sunday 7 days" $ afterDays Sunday 7 `shouldBe` Sunday
      it "Monday after Sunday 8 days" $ afterDays Sunday 8 `shouldBe` Monday
      it "Friday after Monday 228 days" $ afterDays Monday 228 `shouldBe` Friday
      it "Friday after Friday 0 days" $ afterDays Friday 0 `shouldBe` Friday

    describe "isWeekend" $ do
      it "Saturday is weekend" $ isWeekend Saturday `shouldBe` True
      it "Sunday is weekend" $ isWeekend Sunday `shouldBe` True
      it "Monday isn't weekend" $ isWeekend Monday `shouldBe` False
      it "Tuesday isn't weekend" $ isWeekend Tuesday `shouldBe` False
      it "Wednesday isn't weekend" $ isWeekend Wednesday `shouldBe` False
      it "Thursday isn't weekend" $ isWeekend Thursday `shouldBe` False
      it "Friday isn't weekendt too :(" $ isWeekend Friday `shouldBe` False

    describe "days to Friday" $ do
      it "6 days from Saturday to Friday" $ daysToParty Saturday `shouldBe` 6
      it "5 days from Sunday to Friday" $ daysToParty Sunday `shouldBe` 5
      it "4 days from Monday to Friday" $ daysToParty Monday `shouldBe` 4
      it "3 days from Tuesday to Friday" $ daysToParty Tuesday `shouldBe` 3
      it "2 days from Wednesday to Friday" $ daysToParty Wednesday `shouldBe` 2
      it "1 days from Thursday to Friday" $ daysToParty Thursday `shouldBe` 1
      it "0 days from Friday to Friday" $ daysToParty Friday `shouldBe` 0
