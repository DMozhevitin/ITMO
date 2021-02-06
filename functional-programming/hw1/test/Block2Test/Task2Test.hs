module Block2Test.Task2Test
  ( test
  ) where

import           Block2.Task2
import           Data.List.NonEmpty
import           Test.Tasty         (TestTree)
import           Test.Tasty.Hspec   (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "splitOn and joinWith" $ do
    describe "splitOn test" $ do
        it "sample test" $ splitOn '/' "path/to/file" `shouldBe` ("path" :| ["to", "file"])
        it "splitOn '/' \"\" = [] :| []" $ splitOn '/' "" `shouldBe` ([] :| [])
    describe "joinWith test" $ do
        it "sample test" $ joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"
        it "joinWith '/' ([] :| [[]]) = \"/\"" $ joinWith '/' ([] :| [[]]) `shouldBe` "/"
    describe "joinWith and splitOn test" $ do
        it "joinWith x . splitOn x = id" $ ((joinWith "/") . (splitOn "/")) ["path/to/file"] `shouldBe` ["path/to/file"]
