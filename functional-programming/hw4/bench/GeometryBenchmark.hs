module GeometryBenchmark where

import Task1 (Point(..), Polygon, perimeter, doubleArea)
import Task1Strict (Point(..), Polygon, perimeter, doubleArea)
import Criterion

strictPolygon :: Task1Strict.Polygon
strictPolygon = map (\a -> Task1Strict.Point2D a a) [1..10000000]

lazyPolygon :: Task1.Polygon
lazyPolygon = map (\a -> Task1.Point2D a a) [1..10000000]

lazyPerimeter :: Benchmark
lazyPerimeter = bench "lazy perimeter on polygon with 10_000_000 points" $ nf Task1.perimeter lazyPolygon

strictPerimeter :: Benchmark
strictPerimeter = bench "strict perimeter on polygon with 10_000_000 points" $ nf Task1Strict.perimeter strictPolygon

lazyDoubleArea :: Benchmark
lazyDoubleArea = bench "lazy double area on polygon with 10_000_000 points" $  nf Task1.doubleArea lazyPolygon

strictDoubleArea :: Benchmark
strictDoubleArea = bench "strict double area on polygon with 10_000_000 points" $ nf Task1Strict.doubleArea strictPolygon