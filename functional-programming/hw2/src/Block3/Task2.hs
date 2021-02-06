{-# LANGUAGE LambdaCase #-}

module Block3.Task2
  ( ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import Block3.Task1 (Parser(..))
import Control.Applicative (liftA2)

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \case
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy predicate = Parser $ \s -> if liftA2 (&&) (not . null) (predicate . head) s
    then
        Just (head s, tail s)
    else
        Nothing

element :: (Eq s) => s -> Parser s s
element c = satisfy (== c)

stream :: (Eq s) => [s] -> Parser s [s]
stream = traverse element

