module Main
       ( main
       ) where

import Parser(SyntaxTree(..), ParseError(..), ParserT, expr, visualize)
import Lexer(LexicalError(..), Token(..), tokenize)
import Data.List(intercalate)

main :: IO ()
main = do
	s <- getLine
	case tokenize s of
		Left (LexicalError err) -> putStrLn err
		Right ts -> case expr ts of
			Left (ParseError err) -> putStrLn err
			Right (t, _) -> putStrLn $ intercalate "\n" $ visualize t
