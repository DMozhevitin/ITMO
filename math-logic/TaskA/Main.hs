module Main where
import Lex
import Parser

instance Show Expr where
	show (Impl x y) = "(->," ++ show x ++ "," ++ show y ++ ")"
	show (Disj x)   = show x

instance Show Disj where
	show (Dis x y) = "(|," ++ show x ++ "," ++ show y ++ ")"
	show (Conj x  ) = show x

instance Show Conj where
	show (Con x y) = "(&," ++ show x ++ "," ++ show y ++ ")"
	show (Neg x) = show x

instance Show Neg where
	show (Negate x) = "(!" ++ show x ++ ")"
	show (Var x) = x

solve :: String -> String
solve s = show $ parse $ alexScanTokens s

main = interact solve 