module Block3Test.Task3Test
  ( test
  ) where

import           Block3.Task1 (Parser(..))
import           Block3.Task3 (parseCbs, parseInt)
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Task 3 test" $ do
    describe "Correct Bracket Sequences parser tests" $ do
      it "empty string is correct" $ runParser parseCbs "" `shouldBe` Just ((), "")
      it "() is correct" $ runParser parseCbs "()" `shouldBe` Just ((), "")
      it "(()) is correct" $ runParser parseCbs "(())" `shouldBe` Just ((), "")
      it ")(()) isn't correct" $ runParser parseCbs ")(())" `shouldBe` Nothing
      it ") isn't correct" $ runParser parseCbs ")" `shouldBe` Nothing
      it "))(()) isn't correct" $ runParser parseCbs "))(())" `shouldBe` Nothing
      it "()() is correct" $ runParser parseCbs "()()" `shouldBe` Just ((), "")
      it "()(()) is correct" $ runParser parseCbs "()(())" `shouldBe` Just ((), "")
      it "(())() is correct" $ runParser parseCbs "(())()" `shouldBe` Just ((), "")
      it "()()() is correct" $ runParser parseCbs "()()()" `shouldBe` Just ((), "")

    describe "Integer parser tests" $ do
      it "0 is correct" $ runParser parseInt "0" `shouldBe` Just (0, [])
      it "-0 is correct" $ runParser parseInt "-0" `shouldBe` Just (0, [])
      it "+0 is correct" $ runParser parseInt "+0" `shouldBe` Just (0, [])
      
      it "228 is correct" $ runParser parseInt "228" `shouldBe` Just (228, [])
      it "+228 is correct" $ runParser parseInt "+228" `shouldBe` Just (228, [])
      it "-228 is correct" $ runParser parseInt "-228" `shouldBe` Just (-228, [])


      it "1a is correct" $ runParser parseInt "1a" `shouldBe` Just (1, "a")
      it "+1a is correct" $ runParser parseInt "+1a" `shouldBe` Just (1, "a")
      it "-1a is correct" $ runParser parseInt "-1a" `shouldBe` Just (-1, "a")

      it "0xff is correct" $ runParser parseInt "0xff" `shouldBe` Just (0, "xff")
      it "+0xff is correct" $ runParser parseInt "+0xff" `shouldBe` Just (0, "xff")
      it "-0xff is correct" $ runParser parseInt "-0xff" `shouldBe` Just (0, "xff")

      it "x123 isn't correct" $ runParser parseInt "x123" `shouldBe` Nothing
      it "+x123 isn't correct" $ runParser parseInt "+x123" `shouldBe` Nothing
      it "-x123 isn't correct" $ runParser parseInt "-x123" `shouldBe` Nothing