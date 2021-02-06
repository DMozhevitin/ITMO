module Main where

import           Test.Tasty           (defaultMain, testGroup)

import qualified Block1Test.Task1Test (test)
import qualified Block1Test.Task2Test (test)
import qualified Block1Test.Task3Test (test)

import qualified Block2Test.Task1Test (test)
import qualified Block2Test.Task2Test (test)

import qualified Block3Test.Task2Test (test)
import qualified Block3Test.Task3Test (test)

main :: IO ()
main = do
    testBlock1Task1 <- Block1Test.Task1Test.test
    testBlock1Task2 <- Block1Test.Task2Test.test
    testBlock1Task3 <- Block1Test.Task3Test.test
    
    testBlock2Task1 <- Block2Test.Task1Test.test
    testBlock2Task2 <- Block2Test.Task2Test.test

    testBlock3Task2 <- Block3Test.Task2Test.test
    testBlock3Task3 <- Block3Test.Task3Test.test
    
    defaultMain $ testGroup "All Tests" [testGroup "Block 1" [testBlock1Task1, testBlock1Task2, testBlock1Task3],
                                        testGroup "Block 2" [testBlock2Task1, testBlock1Task2],
                                        testGroup "Block 3" [testBlock3Task2, testBlock3Task3]
                                        ]