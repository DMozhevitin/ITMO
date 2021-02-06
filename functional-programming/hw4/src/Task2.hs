module Task2 where

import System.Random
import Control.Monad.Par.Combinator
import Control.Monad.Par
import qualified Data.Vector.Unboxed as V hiding (foldr)
import Data.Vector.Unboxed ((!))

type DoubleFunction = Double -> Double

sampleFunction :: DoubleFunction
sampleFunction x = foldr (\a b -> a + (cos x) ** b) 0 [1..100]
    
monteCarloParallel :: DoubleFunction -> (Double, Double) -> Int -> IO Double
monteCarloParallel f (a, b) n = do
    g <- getStdGen
    let points = V.fromList $ take n $ (randomRs (a, b) g) 
    let s = runPar $ parMapReduceRangeThresh 100 (InclusiveRange 0 (n - 1)) 
                                                 (\x -> return (f $ points ! x)) 
                                                 (\x y -> return (x + y)) 0
    let res = (b - a) / (fromIntegral n) * s
    return res

monteCarloSequential :: DoubleFunction -> (Double, Double) -> Int -> IO Double
monteCarloSequential f (a, b) n = do
    g <- getStdGen
    let points = take n $ (randomRs (a, b) g)
    let s = foldr (\x y -> x + f y) 0 points
    let res = (b - a) / (fromIntegral n) * s
    return res
