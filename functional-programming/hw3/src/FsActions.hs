{-# LANGUAGE FlexibleInstances #-}

module FsActions where

import System.Directory (Permissions (..), canonicalizePath, createDirectory, findFile, getFileSize,
                         getPermissions, listDirectory, removeDirectoryRecursive, removeFile)

import System.FilePath.Posix (splitDirectories)

import Control.Monad.Reader

import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (writeFile)
import System.FilePath ((</>))

class (Monad m) => FsActions m where
    pwd :: m FSResult

    mkdir :: FilePath -> m FSResult

    cd :: FilePath -> m FSResult

    ls :: FilePath -> m FSResult

    dir :: m FSResult

    rm :: FilePath -> m FSResult

    rmdir :: FilePath -> m FSResult

    touch :: FilePath -> m FSResult

    stat :: FilePath -> m FSResult

    cat :: FilePath -> m FSResult

    writeFile :: FilePath -> T.Text -> m FSResult

    find :: FilePath -> m FSResult

instance FsActions (ReaderT (IORef FilePath) IO) where
    cd p = do
        ref <- ask
        currentDirectory <- liftIO $ readIORef ref

        newPath <- liftIO $ resolvePath currentDirectory p
        liftIO $ writeIORef ref newPath
        return Unit

    pwd = do
        ref <- ask
        d <- liftIO $ readIORef ref
        return $ FilePathResult d

    rm p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p
        liftIO $ removeFile resolvedPath
        return Unit

    mkdir p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p
        liftIO $ createDirectory resolvedPath
        return Unit

    ls p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p
        listDir <- liftIO $ listDirectory resolvedPath
        return $ FilePathsResult listDir

    dir = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        ls currentDir

    rmdir p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p
        liftIO $ removeDirectoryRecursive resolvedPath
        return Unit

    touch p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p
        liftIO $ TIO.writeFile resolvedPath $ T.pack ""
        return Unit

    stat p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p

        let n = last $ splitDirectories resolvedPath
        pth         <- liftIO $ canonicalizePath resolvedPath
        perms     <- liftIO $ getPermissions resolvedPath
        sz        <- liftIO $ getFileSize resolvedPath
        c         <- liftIO $ TIO.readFile resolvedPath

        return $ FileDescResult FileDesc        { fname = n
                                                 , path = pth
                                                 , permissions = perms
                                                 , size = sz
                                                 , content = c
                                                 }

    cat p = do
        ref <- ask
        _ <- liftIO $ readIORef ref

        c <- liftIO $ TIO.readFile p

        return $ TextResult c

    writeFile p c = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref
        resolvedPath <- liftIO $ resolvePath currentDir p

        liftIO $ TIO.writeFile resolvedPath c

        return Unit

    find p = do
        ref <- ask
        currentDir <- liftIO $ readIORef ref

        f <- liftIO $ findFile [currentDir] p
        case f of
            Just pth  -> return $ FilePathResult pth
            Nothing -> return $ FilePathResult "File not found"

data FSResult
    = Unit
    | FilePathResult FilePath
    | FilePathsResult [FilePath]
    | FileDescResult FileDesc
    | TextResult T.Text

instance Show FSResult where
    show Unit                  = ""
    show (FilePathResult fp)   = fp
    show (FilePathsResult fps) = show fps
    show (FileDescResult fd)   = show fd
    show (TextResult t)        = show t


data FileDesc = FileDesc
    { fname       :: FilePath
    , path        :: FilePath
    , permissions :: Permissions
    , size        :: Integer
    , content     :: T.Text
    } deriving (Show, Eq)

data DirDesc = DirDesc
   { dirname        :: FilePath
   , dirpath        :: FilePath
   , dirPermissions :: Permissions
   } deriving (Show, Eq)

resolvePath :: FilePath -> FilePath -> IO FilePath
resolvePath p1 p2 = canonicalizePath $ p1 </> p2
