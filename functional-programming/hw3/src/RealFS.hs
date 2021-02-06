module RealFS where

import Control.Monad.Except (catchError)
import Control.Monad.Reader
import Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import FsActions (FSResult (..), FsActions (..))
import Options.Applicative (execParserPure, getParseResult, prefs, showHelpOnEmpty)
import Parser (Command (..), options)
import Prelude hiding (writeFile)
import System.IO (hFlush, stdout)

cli :: ReaderT (IORef FilePath) IO ()
cli = do
    ref <- ask
    curDir <- liftIO $ readIORef ref
    liftIO $ putStr $ curDir ++ ">"
    liftIO $ hFlush stdout

    input <- liftIO getLine
    let command = getParseResult $ execParserPure (prefs showHelpOnEmpty) options $ words input
    case command of
        Just Exit -> return ()
        Just cmd -> do
            res <- catchError (execute cmd) (\e -> do
                liftIO $ putStrLn $ "Error: " <> show e
                return Unit)
            liftIO $ print res -- TODO don't print blank if result is ()
        Nothing -> do
            liftIO $ putStrLn "Invalid command"

    when (command /= Just Exit)
        cli

execute :: Command -> ReaderT (IORef FilePath) IO FSResult
execute (Cd path)                = cd path
execute Pwd                      = pwd
execute (Rm path)                = rm path
execute (Ls path)                = ls path
execute (Rmdir path)             = rmdir path
execute (Touch path)             = touch path
execute (Stat path)              = stat path
execute (Cat path)               = cat path
execute (WriteFile path content) = writeFile path $ T.pack (unwords content)
execute (Find path)              = find path
execute Dir                      = dir
execute Help                     = return Unit
execute (Mkdir path)             = mkdir path
execute Exit                     = return Unit
