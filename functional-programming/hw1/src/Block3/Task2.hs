module Block3.Task2 where

data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where
    (<>) (x :| xs) (y :| ys) = x :| (xs <> [y] <> ys)


data ThisOrThat a b = This a | That b | Both a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) (This x) (This y)     = This (x <> y)
    (<>) (This x) (That y)     = Both x y
    (<>) (This x) (Both y z)   = Both (x <> y) z
    (<>) (That x) (That y)     = That (x <> y)
    (<>) (That x) (This y)     = Both y x
    (<>) (That x) (Both y z)   = Both y (x <> z)
    (<>) (Both x y) (This z)   = Both (x <> z) y
    (<>) (Both x y) (That z)   = Both x (y <> z)
    (<>) (Both x y) (Both z w) = Both (x <> z) (y <> w)

newtype Name = Name String deriving (Show)


instance Monoid Name where
    mempty = Name ""

instance Semigroup Name where
    (<>) (Name "") y       =  y
    (<>) x (Name "")       = x
    (<>) (Name x) (Name y) = Name (x ++ "." ++ y)

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
    mempty = (Endo id)
