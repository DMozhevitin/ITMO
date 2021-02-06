{
module Parser where
import Lexer	
}

%name parseProof Expr
%name parseHead 

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
    "|-"    { TTurn  }
    ','     { TCommo }

%%

Head:
    Exprs "|-" Expr     { ($1, $3)   }
    | "|-"     Expr     { ([], $2)   }

Exprs:
    Exprs ',' Expr      { ($3 : $1)  }
    | Expr              { ($1 : [])  }

Expr:
    Disj "->" Expr      { $1 :->: $3 }
    | Disj              { $1         }

Disj:
    Disj '|' Conj       { $1 :|: $3  }
    | Conj              { $1         }

Conj: 
    Conj '&' Unary      { $1 :&: $3  }
    | Unary             { $1         }

Unary:
    '!' Unary           { Neg $2     }
    | var               { Var $1     }
    | '(' Expr ')'      { $2   }

{
parseError :: [Token] -> a
parseError _  = error "Parse error"

data Expr = Expr :&: Expr | Expr :|: Expr | Expr :->: Expr | Neg Expr | Var String deriving (Ord, Eq)
}

