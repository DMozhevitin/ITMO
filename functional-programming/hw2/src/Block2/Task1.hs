module Block2.Task1
  ( Expr(..)
  , ArithmeticError(..),
    eval,
    applyAbstractOp,
    isNegativeRight
  ) where

import Control.Monad (liftM2)

data Expr 
    = Const Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving (Show)

data ArithmeticError
    = DivisionByZero
    | NegativePow
    deriving (Show, Eq)

type ResT = Either ArithmeticError Int

applyAbstractOp :: (ResT -> ResT -> ResT) -> Expr -> Expr -> ResT
applyAbstractOp op x y = eval x `op` eval y

isNegativeRight :: ResT -> Bool
isNegativeRight (Right z) = z < 0
isNegativeRight _         = False

eval :: Expr -> ResT
eval (Const x) = return x
eval (Add x y) = applyAbstractOp (liftM2 (+)) x y
eval (Sub x y) = applyAbstractOp (liftM2 (-)) x y
eval (Mul x y) = applyAbstractOp (liftM2 (*)) x y
eval (Div x y) = case eval y of
    (Right 0)  -> Left DivisionByZero
    _          -> applyAbstractOp (liftM2 div) x y
eval (Pow x y) = if isNegativeRight (eval y)
    then
        Left NegativePow
    else
        applyAbstractOp (liftM2 (^)) x y
        

