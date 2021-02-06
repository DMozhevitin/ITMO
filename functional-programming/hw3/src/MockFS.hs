{-# LANGUAGE FlexibleInstances #-}
module MockFS where

import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import FsActions (FSResult (..), FsActions (..))
import System.FilePath.Posix (isAbsolute, splitDirectories, (</>))

instance FsActions (State (FilePath, FS)) where

    cd path = do
        (curDir, fs) <- get
        let newCurDir = normalise $ resolvePath' curDir path
        put (newCurDir, fs)
        return Unit

    pwd = do
        (curDir, _) <- get
        return $ FilePathResult curDir

    mkdir name = do
        (curDir, fs) <- get
        let path = normalise curDir
        let xs = tail $ splitDirectories path

        let newFs = walk fs xs (createDirectory name)
        put (curDir, newFs)
        return Unit

    touch name = do
        (curDir, fs) <- get
        let path = normalise curDir
        let xs = tail $ splitDirectories path

        let newFs = walk fs xs (createFile name)
        put (curDir, newFs)
        return Unit

    rm name = do
        (curDir, fs) <- get
        let path = normalise curDir
        let xs = splitDirectories path

        let newFs = walk fs xs (remove name)
        put (curDir, newFs)
        return Unit

    rmdir name = do
        (curDir, fs) <- get
        let path = normalise curDir
        let xs = tail $ splitDirectories path

        let newFs = walk fs xs (remove name)
        put (curDir, newFs)
        return Unit

    ls path = do
        (curDir, fs) <- get
        let curDir' = resolvePath' curDir path

        case findPath curDir' fs of
            Just (Directory _ children) -> return $ TextResult $ T.pack $ intercalate "\n" $ map show children
            Just (File _) -> error "This command can be applied only on directory."
            Nothing -> error "Cannot find path."

    dir = do
        (curDir, _) <- get
        ls curDir

    find = undefined -- TODO implement

    stat = undefined

    cat = undefined

    writeFile = undefined

data FS
   = File FilePath
   | Directory  FilePath [FS]
   deriving (Eq, Show)

resolvePath' :: FilePath -> FilePath -> FilePath
resolvePath' p1 p2 =
    if isAbsolute p2 then
        normalise p2
    else
        normalise (p1 </> p2)

walk :: FS -> [FilePath] -> (FS -> FS) -> FS
walk (File _)  _ _  = error "Invalid path."
walk fs        []     f  = f fs
walk (Directory name files) (x : xs) f = Directory name $ map (\file -> if getName file == x then walk file xs f else file) files

createDirectory :: FilePath -> FS -> FS
createDirectory _ (File _)                          = error "Invalid path."
createDirectory newDirectory (Directory name files) = Directory name $ Directory newDirectory [] : files

createFile :: FilePath -> FS -> FS
createFile _ (File _)                     = error "Invalid path."
createFile newFile (Directory name files) = Directory name $ File newFile : files

remove :: FilePath -> FS -> FS
remove _ (File _)                  = error "Invalid path."
remove path (Directory name files) = Directory name $ filter (\f -> getName f /= path) files

normalise :: FilePath -> FilePath
normalise p = normalise' (splitDirectories p) [] where
    normalise' [] res = intercalate "/" $ reverse res
    normalise' (x : xs) res
        | x == ".." && null res = error "Invalid path."
        | x == ".." = normalise' xs $ tail res
        | x == "." = normalise' xs res
        | x == "/" = normalise' xs res
        | otherwise = normalise' xs (x : res)

findPath :: FilePath -> FS -> Maybe FS
findPath p d@(Directory dirPath children)
    | p == dirPath = Just d
    | otherwise = case xs of
        (x : _) -> x
        []      -> Nothing
        where xs = filter isJust $ map (findPath p) children
findPath p f@(File fp)
    | p == fp = Just f
    | otherwise = Nothing

getName :: FS -> FilePath
getName (Directory name _) = name
getName (File name)        = name
