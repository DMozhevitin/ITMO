module Block3.Task3
  ( parseCbs
  , parseInt
  ) where

import Block3.Task1 (Parser(..))
import Block3.Task2 (ok, satisfy, element, eof)

import Data.Char (isDigit)
import Control.Applicative ((<|>), some)

-- | (S)S | eps - grammar for correct bracket sentences.
parseCbs :: Parser Char ()
parseCbs = parseCbs' <* eof where
    parseCbs' = (open *> parseCbs' *> close *> parseCbs') <|> ok   
    open = element '('
    close = element ')'

parseInt :: Parser Char Int
parseInt = fmap negate (minus *> parseNaturalNumber) <|> plus *> parseNaturalNumber <|> parseNaturalNumber where    

    parseDigit :: Parser Char Char 
    parseDigit = satisfy isDigit

    parseNaturalNumber :: Parser Char Int
    parseNaturalNumber = fmap read (some parseDigit)

    minus :: Parser Char Char
    minus = element '-'

    plus :: Parser Char Char
    plus  = element '+' 