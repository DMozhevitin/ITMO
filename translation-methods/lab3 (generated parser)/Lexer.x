{
module Lexer where    
}

%wrapper "basic"

$digit=0-9
$alpha=[a-zA-Z]

tokens :-
    $white                             ;
    \+                { \s -> TPlus    } 
    \-                { \s -> TMinus   } 
    \*                { \s -> TMul     } 
    \/                { \s -> TDiv     }
    \&                { \s -> TAnd     }
    \|                { \s -> TOr      }
    \True                { \s -> TTrue    }
    \False                { \s -> TFalse   }
    \>                { \s -> TG       }
    \<                { \s -> TL       }
    \<=               { \s -> TLE       }
    \>=               { \s -> TGE       }
    \==               { \s -> TEQ       }
    \!=               { \s -> TNEQ      }

    \if               {  \s -> TIF       }
    \else             {  \s -> TELSE     }
    [$digit]+         {  \s -> TConst (read s) }

    \=                { \s -> TAssign   }
    $alpha            { \s -> TVar s }

    \print            { \s -> TPrint }   


{
data Token = TPlus 
           | TMinus 
           | TMul 
           | TDiv 
           | TConst Int 
           | TAnd
           | TOr
           | TTrue
           | TFalse
           | TG
           | TL
           | TLE
           | TGE
           | TEQ
           | TNEQ
           | TIF
           | TELSE
           | TNONE
           | TVar String
           | TAssign
           | TPrint
           deriving (Eq, Show)
}