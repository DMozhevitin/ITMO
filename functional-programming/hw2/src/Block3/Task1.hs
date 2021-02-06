module Block3.Task1 where

import Control.Applicative (Alternative, (<|>), empty)

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) } -- newtype instead of data to avoid warning

instance Functor (Parser s) where
    fmap f (Parser p) = Parser $ \s -> do
        (a, b) <- p s
        return (f a, b)


instance Applicative (Parser s) where
    pure x = Parser $ \s -> Just (x, s)

    (<*>) (Parser f) (Parser p) = Parser $ \s -> do
        (h, s') <- f s
        (b, s'') <- p s'
        return (h b, s'')

instance Monad (Parser s) where
    return x = Parser $ \s -> Just (x, s)
    (>>=) (Parser p) f = Parser $ \s -> do
        (r, s') <- p s
        runParser (f r) s'

instance Alternative (Parser s) where
    empty = Parser $ const Nothing

    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s    
