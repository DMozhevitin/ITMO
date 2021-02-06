{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Task1Strict 
  ( Point(..)
  , Polygon
  , perimeter
  , doubleArea
  , plus
  , minus
  , scalarProduct
  ) where

data Point = Point2D
    { x :: Int
    , y :: Int 
    } deriving (Show)

type Polygon = [Point]

plus :: Point -> Point -> Point
plus (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point2D x1 y1) (Point2D x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point2D x1 y1) (Point2D x2 y2) = x1 * y2 - x2 * y1

distance :: Point -> Point -> Double
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt $! fromIntegral $! dx + dy where
    !dx = (x1 - x2) * (x1 - x2)
    !dy = (y1 - y2) * (y1 - y2)

perimeter :: Polygon -> Double
perimeter = reducePolygon $! distance

doubleArea :: Polygon -> Int
doubleArea = reducePolygon $! crossProduct 

reducePolygon :: (Num a) => (Point -> Point -> a) -> Polygon -> a
reducePolygon f !polygon = snd $ foldr (\p (!q, !acc) -> (p, acc + p `f` q)) (start, 0) polygon where
    start = case polygon of
        (a : _)   -> a
        _         -> error "Polygon is empty."