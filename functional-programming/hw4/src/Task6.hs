module Task6
  ( FS(..)
  , name
  , contents
  , _File
  , _Dir
  , getDirectory'
  ) where

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
-- import System.FilePath.Posix ((</>), splitDirectories) -- use right module for your OS
import System.FilePath.Windows ((</>), splitDirectories)
import Control.Monad (filterM)
import Lens.Micro (Lens', Traversal', lens)

data FS 
    = Dir 
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath
    } deriving (Show)

getDirectory' :: FilePath -> IO FS
getDirectory' path = do
    doesDirExist <- doesDirectoryExist path
    if doesDirExist then do
        listDir <- listDirectory path

        childrenDirs <- filterM (doesDirectoryExist . ((</>) path)) listDir
        childrenFiles <- filterM (doesFileExist . ((</>) path)) listDir

        let files = map File childrenFiles
        dirs <- mapM (\f -> getDirectory' $ path </> f) childrenDirs
        
        let shortPath = last $ splitDirectories path
        
        return $ Dir { _name = shortPath, _contents = (files ++ dirs) }
    else 
        error "Directory with given name does not exist."

name :: Lens' FS FilePath
name = lens _name (\f name' -> f { _name = name' })

contents :: Traversal' FS [FS]
contents = _Dir . lens _contents (\f p -> f { _contents = p })

-- | microlens package doesn't provide prisms, 
--   because prism is the same as Traversal' with 0 or 1 values
--   we just use 'pure' in wrong constructor. in case of Maybe it will be Nothing, like in Prism'
_File :: Traversal' FS FS
_File f file@(File _) = f file
_File _ dir           = pure dir

_Dir :: Traversal' FS FS
_Dir f dir@(Dir _ _) = f dir
_Dir _ file        = pure file
