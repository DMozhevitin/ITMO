module Block1.Task2 
  ( Tree(..) 
  ) where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a 
  deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
    pure = Leaf
    (Leaf f) <*> (Leaf x) = Leaf (f x)
    (Branch l r) <*> right@(Leaf _) = Branch (l <*> right) (r <*> right)
    left@(Leaf _) <*> (Branch l r) = Branch (left <*> l) (left <*> r)
    (Branch l r) <*> (Branch l' r') = Branch (l <*> l') (r <*> r')

instance Foldable Tree where
    foldr f acc (Leaf x) = f x acc
    foldr f acc (Branch l r) = foldr f (foldr f acc r) l

instance Traversable Tree where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Branch x y) = Branch <$> traverse f x <*> traverse f y