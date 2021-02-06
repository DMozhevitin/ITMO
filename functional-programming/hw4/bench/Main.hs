module Main where

import Criterion.Main
import GeometryBenchmark (lazyPerimeter, strictPerimeter, strictDoubleArea, lazyDoubleArea)
import MonteCarloBenchmark (sequentialBenchmark, parallelBenchmark)

main :: IO ()
main = defaultMain [
    bgroup "Geometry benchmark" 
                 [ lazyPerimeter
                 , strictPerimeter
                 , lazyDoubleArea
                 , strictDoubleArea ],
    bgroup "Monte Carlo benchmark" 
                 [ sequentialBenchmark
                , parallelBenchmark ]
    ]