{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
        '+' { TPlus }
        '-' { TMinus }
        '*' { TMul }
        '/' { TDiv }
        cnst { TConst $$}

        '|' { TOr }
        '&' { TAnd }
        'True'  { TTrue }
        'False'  { TFalse }

        '>'    { TG }
        '<'    { TL }
        '<='   { TLE }
        '>='   { TGE }
        '=='   { TEQ }
        '!='   { TNEQ }

        'if'   { TIF }
        'else' { TELSE }

        '='     { TAssign }
        var     { TVar $$}

        'print' { TPrint }
%%

Root:
    If { RootIf $1 }
    | Statement { RootSt $1 }

If:
    'if' Logic Statement 'else' If   { IfElifExpr $2 $3 $5   }
    | 'if' Logic Statement 'else' Statement { IfExpr $2 $3 (Just $5) }
    | 'if' Logic Statement    { IfExpr $2 $3 Nothing }

Statement:
    Print          { StPrint  $1 }
    | Expr         { StExpr   $1 }
    | '=' var Expr { StAssign $2 $3 }


Print:
    'print' Expr { Print $2 }

Expr: 
    Arithm       { Arithmetic $1 }
    | Logic      { Logic  $1 }

Logic:
    Cmp                 { $1        }
    | '&' Logic Logic   { $2 :&: $3 }
    | '|' Logic Logic   { $2 :|: $3 }
    | LogicUnary        { $1        }

LogicUnary:
    'True'              { BoolValue True }
    | 'False'            { BoolValue False }  

Arithm:
    '+' Arithm Arithm   { $2 :+: $3 }
    | '-' Arithm Arithm { $2 :-: $3 }
    | '*' Arithm Arithm { $2 :*: $3 }
    | '/' Arithm Arithm { $2 :/: $3 }
    | Unary             { $1        }

Cmp:
    '<' Arithm Arithm      { $2 :<: $3 }
    | '>' Arithm Arithm    { $2 :>: $3 }
    | '>=' Arithm Arithm   { $2 :>=: $3 }
    | '<=' Arithm Arithm   { $2 :<=: $3 }
    | '==' Arithm Arithm   { $2 :==: $3 }
    | '!=' Arithm Arithm   { $2 :!=: $3 }

Unary:
    cnst                  { Const $1 } 
    | var                 { Var   $1 }           

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data IfExpr = IfExpr LogicExpr Statement (Maybe Statement)
            | IfElifExpr LogicExpr Statement IfExpr
            deriving (Eq)

data Statement = StPrint Print
               | StExpr Expr
               | StAssign String Expr
               deriving (Eq)

data Print = Print Expr
           deriving (Eq)

data ArithmExpr = ArithmExpr :+: ArithmExpr 
                | ArithmExpr :-: ArithmExpr 
                | ArithmExpr :*: ArithmExpr 
                | ArithmExpr :/: ArithmExpr 
                | Const Int 
                | Var String
                deriving (Eq)

data LogicExpr = LogicExpr :&: LogicExpr
               | LogicExpr :|: LogicExpr
               | BoolValue Bool 
               | ArithmExpr :<: ArithmExpr
               | ArithmExpr :>: ArithmExpr
               | ArithmExpr :>=: ArithmExpr
               | ArithmExpr :<=: ArithmExpr
               | ArithmExpr :==: ArithmExpr
               | ArithmExpr :!=: ArithmExpr
               deriving (Eq)

data Expr     = Arithmetic ArithmExpr
              | Logic LogicExpr
              deriving (Eq)

data Root = RootSt Statement
         | RootIf IfExpr
         deriving (Eq)
}