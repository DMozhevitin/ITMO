module Main where

import Lexer
import Parser

f :: String -> String
f s = show $ parse $ alexScanTokens s


instance Show Root where
    show (RootSt x) = show x
    show (RootIf x) = show x


instance Show Expr where
    show (Arithmetic e) = show e
    show (Logic e)      = show e

instance Show ArithmExpr where
    show (a :+: b) = show a ++ " + " ++ show b
    show (a :-: b) = show a ++ " - " ++ show b
    show (a :*: b) = show a ++ " * " ++ show b
    show (a :/: b) = show a ++ " / " ++ show b
    show (Const x) = show x
    show (Var v)   = v

instance Show LogicExpr where
    show (a :|: b) = show a ++ " or " ++ show b
    show (a :&: b) = show a ++ " and " ++ show b
    show (BoolValue x) = show x
    show (a :<: b) = show a ++ " < " ++ show b
    show (a :>: b) = show a ++ " > " ++ show b
    show (a :<=: b) = show a ++ " <= " ++ show b
    show (a :>=: b) = show a ++ " >= " ++ show b
    show (a :==: b) = show a ++ " == " ++ show b
    show (a :!=: b) = show a ++ " != " ++ show b

instance Show Print where
    show (Print e) = "print(" ++ show e ++ ")"

instance Show Statement where
    show (StPrint x) = show x
    show (StExpr e) = show e
    show (StAssign v e) = v ++ " = " ++ show e


instance Show IfExpr where
    show (IfExpr cond e1 (Just e2)) = "if " ++ show cond ++ ":\n" ++ "    " ++ show e1 ++ "\nelse:\n    " ++ show e2
    show (IfExpr cond e1 Nothing) = "if " ++ show cond ++ ":\n" ++ "    " ++ show e1
    show (IfElifExpr cond e1 if2) = "if " ++ show cond ++ ":\n" ++ "    " ++ show e1 ++ "\nel" ++ show if2

main :: IO ()
main = do
    s <- readFile "input.txt"
    putStrLn $ f s

