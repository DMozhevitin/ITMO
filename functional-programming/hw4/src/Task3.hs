{-# LANGUAGE BlockArguments #-}

module Task3 where

import Data.Hashable
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Foldable as F
import Control.Monad (when)
import Data.Vector ((!))
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar, writeTVar)
import Control.Monad.STM (atomically, STM)
import Data.Maybe (fromMaybe)

type Bucket k v = TVar (Maybe [(k, v)])
type HashTable k v = V.Vector (Bucket k v)

data ConcurrentHashTable k v = ConcurrentHashTable (TVar (HashTable k v)) (TVar Int)

initialCapacity :: Int
initialCapacity = 16

rehashThreshold :: Double
rehashThreshold = 0.8

newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
    table <- V.replicateM initialCapacity $ newTVar Nothing
    tableTVar <- newTVar table
    curSize <- newTVar (0 :: Int)
    return $ ConcurrentHashTable tableTVar curSize


sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable _ sz) = atomically $ readTVar sz

getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable elems _) = atomically $ do
    let h = hash key
    table <- readTVar elems
    let size = V.length table
    bucket <- readTVar (table ! (h `mod` size))

    case bucket of
        Nothing -> return Nothing
        Just b  -> return $ (L.find (\(k, _) -> k == key) b) >>= (return .snd)

putCHT :: (Eq k, Hashable k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable elems sz) = atomically $ do
    let h = hash key
    curSize <- readTVar sz

    table' <- readTVar elems
    let capacity' = V.length table'

    when (isNeedRehash (curSize + 1) capacity') do
        newTable <- rehash table'
        writeTVar elems newTable

    table <- readTVar elems
    let capacity = V.length table

    let bucketTVar = table ! (h `mod` capacity)
    maybeBucket <- readTVar bucketTVar
    let bucket = fromMaybe [] maybeBucket
    let e = L.find (\(k, _) -> k == key) bucket

    case e of
        Nothing -> do 
            writeTVar bucketTVar $ Just $ ((key, value) : bucket)
            modifyTVar sz succ
        _       -> writeTVar bucketTVar $ Just $ (key, value) : (filter (\(k, _) -> k /= key) bucket)

    return ()


isNeedRehash :: Int -> Int -> Bool
isNeedRehash size capacity = (fromIntegral size) / (fromIntegral capacity) >= rehashThreshold

rehash :: (Hashable k, Eq k) => HashTable k v -> STM (HashTable k v)
rehash oldData = do
    let capacity = V.length oldData
    let newCapacity = 2 * capacity
    newData <- V.replicateM newCapacity $ newTVar Nothing

    V.forM_ oldData (\eTVar -> do
        e <- readTVar eTVar
        case e of
            Nothing -> return ()
            Just x -> F.forM_ x (\(a, b) -> do
                let newIndex = (hash a) `mod` newCapacity
                let tVar = newData ! newIndex
                modifyTVar tVar (\q -> case q of
                    Nothing -> Just [(a, b)]
                    Just qs -> Just ((a, b) : qs))
               ))

    return newData
