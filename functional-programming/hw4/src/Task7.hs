{-# LANGUAGE RankNTypes #-}

module Task7 where

import Lens.Micro (Traversal', (^.), filtered, each)
import Task6 (FS(..), name, contents, _Dir, _File)

ls :: Traversal' FS FilePath
ls = _Dir . contents . each . name

file :: FilePath -> Traversal' FS FilePath
file f =  _Dir . contents . each . _File . filtered ((== f) .  (^. name)) . name

cd :: FilePath -> Traversal' FS FS
cd d = _Dir . contents . each . filtered ((== d) . (^. name))