module Parser
       (expr,
        SyntaxTree(..),
        ParseError(..),
        ParserT,
        visualize
       ) where

import Lexer (Token(..), tokenize, LexicalError(..))

data SyntaxTree = Node String [SyntaxTree] deriving (Show, Eq)

newtype ParseError = ParseError String deriving (Show, Eq)

type ParserT = [Token] -> Either ParseError (SyntaxTree, [Token])

isVar :: Token -> Bool
isVar (Var _) = True
isVar _       = False


expr :: ParserT
expr tokens@(t : _) = if (t == OPEN_BRACKET || (isVar t) || t == NOT) then
	do
		(d, ts') <- disj tokens
		(e', ts'') <- expr' ts'
		return (Node "Expr" [d, e'], ts'')
	else
		Left $ ParseError ("unexpected token " ++ (show t))

expr' :: ParserT
expr' tokens@(t1 : t2 : ts) = if (t1 == XOR || t1 == NOT) then
	do
		let x = Node "^" []
		(d, ts') <- disj (t2 : ts)
		(e', ts'') <- expr' ts'
		return (Node "Expr'" [x, d, e'], ts'')
	else if (t1 == CLOSE_BRACKET || t1 == END) then
		return (Node "Expr'" [], tokens)
	else
		Left $ ParseError ("unexpected token " ++ (show t1))


disj :: ParserT
disj tokens@(t : ts) = if (t == OPEN_BRACKET || (isVar t) || t == NOT) then
	do
		(c, ts') <- conj tokens
		(d', ts'') <- disj' ts'
		return (Node "Disj" [c, d'], ts'')
	else
		Left $ ParseError ("unexpected token " ++ (show t))


disj' :: ParserT
disj' tokens@(t1 : t2 : ts) = if (t1 == OR || t1 == NOT) then
	do
		let x = Node "|" []
		(c, ts') <- conj (t2 : ts)
		(d', ts'') <- disj' ts'
		return (Node "Disj'" [x, c, d'], ts'')
	else if (t1 == XOR || t1 == CLOSE_BRACKET || t1 == END) then
		return (Node "Disj'" [], tokens)
	else
		Left $ ParseError ("unexpected token " ++ (show t1))


conj :: ParserT
conj tokens@(t : ts) = if (t == OPEN_BRACKET || (isVar t) || t == NOT) then
	do
		(n, ts') <- neg tokens
		(c', ts'') <- conj' ts'
		return (Node "Conj" [n, c'], ts'')
	else
		Left $ ParseError ("unexpected token " ++ (show t))


conj' :: ParserT
conj' tokens@(t1 : t2 : ts) = if (t1 == AND || t1 == NOT) then
	do
		let x = Node "&" []
		(n, ts') <- neg (t2 : ts)
		(c', ts'') <- conj' ts'
		return (Node "Conj'" [x, n, c'], ts'')
	else if (t1 == OR || t1 == XOR || t1 == CLOSE_BRACKET || t1 == END) then
		return (Node "Conj'" [], tokens)
	else
		Left $ ParseError ("unexpected token " ++ (show t1))


neg :: ParserT
neg tokens@(t : ts) = if (t == NOT) then
	do
		let x = Node "!" []
		(n', ts') <- neg' ts
		return (Node "Neg" [x, n'], ts')
	else if (t == OPEN_BRACKET) then 
		do
            let x = (Node "(" [])
            (e, ts') <- expr ts

            case ts' of
                [] -> Left $ ParseError "unexpected token"
                (y : ys) -> if (y == CLOSE_BRACKET) then
                    return (Node "Neg" [x, e, (Node ")" [])], ys)
               	else 
                    Left $ ParseError "unexpected token"
	else if (isVar t) then
		return (Node (show t) [], ts)
	else
		Left $ ParseError ("unexpected token " ++ (show t))


neg' :: ParserT
neg' tokens@(t : ts) = if (t == OPEN_BRACKET) then 
		do
            let x = (Node "(" [])
            (e, ts') <- expr ts

            case ts' of
                [] -> Left $ ParseError "unexpected token"
                (y : ys) -> if (y == CLOSE_BRACKET) then
                    return (Node "Neg'" [x, e, (Node ")" [])], ys)
               	else 
                    Left $ ParseError "unexpected token"
	else if (isVar t) then
		return (Node (show t) [], ts)
	else
		Left $ ParseError ("unexpected token " ++ (show t))


visualize :: SyntaxTree -> [String]
visualize t = ["digraph vis {"] ++ visualize' t "x" ++ ["}"]

visualize' :: SyntaxTree -> String -> [String]
visualize' (Node label children) pref = (getLabel pref label) : (concatMap (\(t, i) -> ((visualize' t $ getName pref i) ++ ["    " ++ pref ++ " -> " ++ (getName pref i) ++ ";"])) $ zipWithIndex children)

getLabel :: String -> String -> String
getLabel pref label = "    " ++ pref ++ " [label=\"" ++ label ++ "\"];"

getName :: String -> Int -> String
getName pref i = pref ++ "y" ++ (show i)

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex xs = zip xs [0..]