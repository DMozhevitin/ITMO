module Block3Test.Task2Test
  ( test
  ) where

import           Block3.Task1 (Parser(..))
import           Block3.Task2 (ok, eof, satisfy, element, stream)
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Task 2 test" $ do
    describe "ok parser tests" $ do
      it "ok parser accepts any String #1" $ runParser ok "" `shouldBe` Just ((), "")
      it "ok parser accepts any String #2" $ runParser ok "1" `shouldBe` Just ((), "1")
      it "ok parser accepts any String #3" $ runParser ok "228" `shouldBe` Just ((), "228")
    
      it "ok parser accepts any [Int] #2" $ runParser ok [1] `shouldBe` Just ((), [1])
      it "ok parser accepts any [Int] #3" $ runParser ok [2, 2, 8] `shouldBe` Just ((), [2, 2, 8])

    describe "eof parser tests" $ do
      it "eof parser doesn't accept non empty String" $ runParser eof "1" `shouldBe` Nothing
      it "eof parser acccepts empty String" $ runParser eof "" `shouldBe` Just ((), "")

    describe "satisfy parser tests" $ do
      it "satisfy on always False predicate fails on any input" $ (runParser $ satisfy (const False)) "" `shouldBe` Nothing
      it "satisfy on always True predicate fails on empty input" $ (runParser $ satisfy (const True)) "" `shouldBe` Nothing
      it "satisfy on always True predicate accepts any non empty input #1" $ (runParser $ satisfy (const True)) "1" `shouldBe` Just ('1', "")
      it "satisfy on always True predicate accepts any non empty input #2" $ (runParser  $ satisfy (const True)) "123" `shouldBe` Just ('1', "23")
      it "satisfy even accepts even numbers" $ (runParser $ satisfy even) [2, 3, 4] `shouldBe` Just (2, [3, 4])
      it "satisfy even fails on od numbers" $ (runParser $ satisfy even) [1, 3, 4] `shouldBe` Nothing
      it "satisfy (== 'a') accepts abc" $ (runParser $ satisfy (== 'a')) "abc" `shouldBe` Just ('a', "bc")

    describe "element parser tests" $ do
      it "element 'a' parser accepts abc" $ (runParser $ element 'a') "abc" `shouldBe`  Just ('a', "bc")
      it "element 'a' parser fails on empty input" $ (runParser $ element 'a') "" `shouldBe` Nothing
      it "element 1 parser accepts [1, 2, 3]" $ (runParser $ element (1 :: Int)) [1, 2, 3] `shouldBe` Just (1, [2, 3])
      it "element 1 parser fails on [2, 2, 3]" $ (runParser $ element (1 :: Int)) [2, 2, 3] `shouldBe` Nothing

    describe "stream parser tests" $ do
      it "stream [] accepts empty input" $ (runParser $ stream "") "" `shouldBe` Just ("", "")
      it "stream [1, 2] accepts [1, 2, 3]" $ (runParser $ stream [1, 2]) [1, 2, 3] `shouldBe` Just ([1, 2], [3])
      it "stream [1, 2, 3] accepts [1, 2, 3]" $ (runParser $ stream [1, 2, 3]) [1, 2, 3] `shouldBe` Just ([1, 2, 3], [])
      it "stream [1, 4, 3] fails on [1, 2, 3]" $ (runParser $ stream [1, 4, 3]) [1, 2, 3] `shouldBe` Nothing