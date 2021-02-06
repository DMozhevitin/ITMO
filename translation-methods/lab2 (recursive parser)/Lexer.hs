module Lexer 
       ( Token(..)
       , tokenize
       , LexicalError(..)
       ) where

import Data.Char

data Token = OPEN_BRACKET
           | CLOSE_BRACKET
           | XOR
           | AND
           | OR
           | NOT
           | END
           | Var Char deriving (Eq)

newtype LexicalError = LexicalError String deriving (Show, Eq)

instance Show Token where
	show OPEN_BRACKET  = "("
	show CLOSE_BRACKET = ")" 
	show XOR           = "^"
	show AND           = "&"
	show OR            = "|"
	show NOT           = "!"
	show END           = ""
	show (Var v)       = show v
 

tokenize :: String -> Either LexicalError [Token]
tokenize s = (traverse getToken $ filter (/= ' ') s) >>= (\s -> return $ s ++ (replicate 2 END))

getToken :: Char -> Either LexicalError Token
getToken c = case c of
	'(' -> Right OPEN_BRACKET
	')' -> Right CLOSE_BRACKET
	'^' -> Right XOR
	'&' -> Right AND
	'|' -> Right OR
	'!' -> Right NOT
	_ -> if isAlpha c then 
		Right $ Var c 
	else 
		Left $ LexicalError $ "unexpected token " ++ [c]
