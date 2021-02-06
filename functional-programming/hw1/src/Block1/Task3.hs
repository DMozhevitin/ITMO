{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 ( Tree(..)
                    , isEmpty
                    , size
                    , contains
                    , Block1.Task3.insert
                    , fromList
                    , Block1.Task3.toList
                    , remove
                    ) where


import           Data.List          as List
import           Data.List.NonEmpty as NonEmpty hiding (filter, fromList)

data Tree a = Leaf | Tree (NonEmpty a) (Tree a) (Tree a) deriving Show

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf          = 0
size (Tree xs l r) = (NonEmpty.length xs) + (size l) + (size r)


contains :: Ord a => (Tree a) -> a -> Bool
contains Leaf _ = False
contains (Tree (x :| _) l r) key
    | key == x  = True
    | key < x   = contains l key
    | otherwise = contains r key


insert :: Ord a => (Tree a) -> a -> (Tree a)
insert Leaf val = Tree (val :| []) Leaf Leaf
insert (Tree xs@(x :| _) l r) val
    | x == val = Tree (val <| xs) l r
    | val < x = Tree (xs) (Block1.Task3.insert l val) r
    | otherwise = Tree (xs) l (Block1.Task3.insert r val)


fromList :: Ord a => [a] -> Tree a
fromList xs = foldl Block1.Task3.insert Leaf xs


toList :: Ord a => Tree a -> [a]
toList Leaf = []
toList (Tree xs l r) = (Block1.Task3.toList l) ++ (NonEmpty.toList xs) ++ (Block1.Task3.toList r)

remove :: Ord a => Tree a -> a -> (Tree a)
remove t x = fromList $ pref ++ suf where
    (pref, suf') = List.span (/= x) $ Block1.Task3.toList t
    suf = case suf' of
        []       -> []
        (_ : xs) -> xs


instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ res Leaf = res
    foldr f acc (Tree xs l r) = foldr f (foldr f (foldr f acc r) (NonEmpty.toList xs)) l


    foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
    foldMap _ Leaf          = mempty
    foldMap f (Tree xs l r) = foldMap f l <> (foldMap f xs) <> foldMap f r
