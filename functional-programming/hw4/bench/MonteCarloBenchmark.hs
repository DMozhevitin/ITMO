module MonteCarloBenchmark (sequentialBenchmark, parallelBenchmark) where

import Task2 (sampleFunction, monteCarloSequential, monteCarloParallel)
import Criterion

suffix :: String
suffix = " Monte Carlo method on 10^6 iterations"

interval :: (Double, Double)
interval = (1, 10)

sequentialBenchmark :: Benchmark
sequentialBenchmark = bench ("sequential" <> suffix) $ nfIO $ monteCarloSequential sampleFunction interval 1000000

parallelBenchmark :: Benchmark
parallelBenchmark = bench ("parallel" <> suffix) $ nfIO $  monteCarloParallel sampleFunction interval 1000000