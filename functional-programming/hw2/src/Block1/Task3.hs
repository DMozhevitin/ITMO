module Block1.Task3
  ( NonEmpty(..)
  ) where

import Control.Applicative (liftA2)

data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Functor NonEmpty where
    fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
    pure f = f :| []
    (f :| fs) <*> (x :| xs) = f x :| tail (gs <*> ys) where
        gs = f : fs
        ys = x : xs

instance Monad NonEmpty where
    return x = x :| []
    (x :| xs) >>= f = y :| (ys ++ (xs >>= (toList .f))) where
        (y :| ys) = f x
        toList (z :| zs) = z : zs

instance Foldable NonEmpty where
    foldr f acc (x :| xs) = f x $ foldr f acc xs

instance Traversable NonEmpty where
    traverse f (x :| xs) = liftA2 (:|) (f x)  (traverse f xs)
