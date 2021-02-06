{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white                      ;
    "|-"                     { \s -> TTurn  }
	"->"                     { \s -> TArrow }
	\!                       { \s -> TNot   }
	\&                       { \s -> TAnd   }
	\|                       { \s -> TOr    }
	\(                       { \s -> TOpBr  }
	\)                       { \s -> TClBr  }
	\,                       { \s -> TCommo }
    $alpha [$alpha $digit \']* { \s -> TVar s }     

{
data Token = TArrow | TNot | TAnd | TOr | TOpBr | TClBr | TTurn | TCommo | TVar String deriving (Eq, Show)
}