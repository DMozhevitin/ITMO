{
module Parser where
import Lex	
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    "->"    { TArrow }
    '!'     { TNot   }
    '&'     { TAnd   }
    '|'     { TOr    }
    var     { TVar $$}
    '('     { TOpBr  }
    ')'     { TClBr  }

%%

Expr:
    Disj "->" Expr     { Impl $1 $3 }
    | Disj             { Disj $1    }

Disj:
    Disj '|' Conj      { Dis $1 $3 }
    | Conj             { Conj $1    }

Conj:
    Conj '&' Neg       { Con $1 $3 }
    | Neg              { Neg $1     }

Neg:
    '!' Neg            { Negate $2     }
    | var              { Var $1     }
    | '(' Expr ')'     { Brack $2   }

{
parseError :: [Token] -> a
parseError _  = error "Parse error"

data Expr = Impl Disj Expr | Disj Disj
data Disj = Dis Disj Conj | Conj Conj
data Conj = Con Conj Neg | Neg Neg
data Neg  = Negate Neg | Var String | Brack Expr	
}