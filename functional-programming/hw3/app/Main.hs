module Main where

import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import RealFS (cli)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    dirRef <- newIORef curDir
    runReaderT cli dirRef
