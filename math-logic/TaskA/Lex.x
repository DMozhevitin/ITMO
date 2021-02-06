{
module Lex where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white                      ;
	"->"                     { \s -> TArrow }
	\!                       { \s -> TNot   }
	\&                       { \s -> TAnd   }
	\|                       { \s -> TOr    }
	\(                       { \s -> TOpBr  }
	\)                       { \s -> TClBr  }
    $alpha [$alpha $digit \']* { \s -> TVar s }     

{
data Token = TArrow | TNot | TAnd | TOr | TOpBr | TClBr | TVar String deriving (Eq, Show)
}