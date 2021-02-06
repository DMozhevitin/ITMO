module Main where
import Lexer
import Parser
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

data Line = MP Int Int Int | Hypot Int Int | Axiom Int Int deriving (Eq, Ord)

instance Show Line where
    show (MP ind a b) =    "[" ++ show ind ++ ". M.P. " ++ show a ++ ", " ++ show b ++ "]"
    show (Hypot ind num) = "[" ++ show ind ++ ". Hypothesis " ++ show num ++ "]"
    show (Axiom ind num) = "[" ++ show ind ++ ". Ax. sch. " ++ show num ++ "]"

instance Show Expr where
    show (a :->: b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"
    show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"
    show (Neg expr) = "!" ++ show expr
    show (Var x) = x

solve :: [String] -> [String]
solve (h : rest) = do
    let (hypotises, toProof) = parseHead $ alexScanTokens h
    let n = length hypotises
    let hypots = Map.fromList (zip (reverse hypotises) [1..n])

    -- sample proofs.
    -- let sample1 = ["A & A -> A", "A -> A -> A", "A -> (A -> A) -> A", "A & A -> A", "(A -> A -> A) -> (A -> (A -> A) -> A) -> A -> A", "(A -> (A -> A) -> A) -> A -> A", "A & A -> A", "A -> A"]
    -- let sample2 = ["A -> B", "!B", "!B -> A -> !B", "A -> !B", "(A -> B) -> (A -> !B) -> !A", "(A -> !B) -> !A", "!A"]
    -- let sample3 = ["B"]

    -- let p = ["a", "a -> b", "b"]
    -- let p1 = ["a", "b", "c"]

    let proof = map (parseProof . alexScanTokens) rest

    case (verifyProof toProof Map.empty hypots Map.empty proof 1 Map.empty) of
        (False, _ ,    _,     _        ) ->  ["Proof is incorrect"]
        (True, proven, impls, ind2Expr) -> do
            let proofPrefix = (takeWhile (/= toProof) proof) ++ [toProof]
            let proof = proofPrefix
            let used = Set.insert toProof (markUsedLines ind2Expr proven (Map.size ind2Expr) Set.empty)
            let usedExprList = (filter (\x -> Set.member x used) proof)
            let uniqExprList = nub usedExprList
            let mapidx = updateIdxs 1 proof used uniqExprList proven Map.empty
            
            h : (reverse (annotateProof uniqExprList proven mapidx used 1 (length uniqExprList) []))
            -- h : (reverse (annotateProof uniqExprList proven uniqLineIndexes mapidx 1 (length usedExprList) []))

main :: IO()
main = interact (unlines . solve . lines)

{- index in proof -}
lineNumber :: Line -> Int
lineNumber (MP ind _ _ ) = ind
lineNumber (Axiom ind _) = ind
lineNumber (Hypot ind _) = ind


{- index of element in list without Maybe -}
indexOf x xs = case (elemIndex x xs) of
    (Just idx) -> idx
    _          -> error (show x)

updateIdxs :: Int -> [Expr] -> Set.Set Expr -> [Expr] -> Map.Map Expr Line -> Map.Map Int Int -> Map.Map Int Int
updateIdxs    _      []             _    usedExprList   _     mapidx = mapidx
updateIdxs   ind (expr : exprs) used usedExprList proven mapidx = case (Set.member expr used) of
    True  -> updateIdxs (ind + 1) exprs used usedExprList proven (Map.insert ind (indexOf expr usedExprList) mapidx)
    False -> updateIdxs (ind + 1) exprs used usedExprList proven mapidx

annotateProof :: [Expr] -> Map.Map Expr Line -> Map.Map Int Int -> Set.Set Expr -> Int -> Int -> [String] -> [String]
annotateProof []             _      _      _         ind _ proof = proof
annotateProof (expr : exprs) proven mapidx usedLines ind n proof = do
    case (proven `get` expr) of
        (MP _ a b) -> do
            let a' = 1 + mapidx `get` a
            let b' = 1 + mapidx `get` b
            -- let a' = a
            -- let b' = b
            let str = show (MP ind a' b') ++ " " ++ show expr
            annotateProof exprs proven mapidx usedLines (ind + 1) n (str : proof)
        (Hypot _ num) -> do
            let str = (show (Hypot ind num)) ++ " " ++ show expr
            annotateProof exprs proven mapidx usedLines (ind + 1) n (str : proof)
        (Axiom _ num) -> do
            let str = (show (Axiom ind num)) ++ " " ++ show expr
            annotateProof exprs proven mapidx usedLines (ind + 1) n (str : proof)

{- Takes expression, number of line and map of proven expressions and return updated map of proven expressions if 
    this expression is axiom or Nothing otherwise. -}
axiom :: Expr -> Int -> Map.Map Expr Line -> Maybe (Map.Map Expr Line)
axiom expr@(a :->: (b :->: a')) ind proven 
 | a == a' = axiom' ind expr proven (Just 1)
axiom expr@((a :->: b) :->: ((a' :->: (b' :->: c)) :->: (a'' :->: c'))) ind proven
 | a == a' && a == a'' && b == b' && c == c' = axiom' ind expr proven (Just 2)
axiom expr@((a :&: b) :->: a') ind proven
 | a == a' = axiom' ind expr proven (Just 4)
axiom expr@((a :&: b) :->: b') ind proven
 | b == b' = axiom' ind expr proven (Just 5)
axiom  expr@(a :->: (b :->: (a' :&: b'))) ind proven 
 | a == a' && b == b' = axiom' ind expr proven (Just 3)
axiom  expr@(a :->: (a' :|: b)) ind proven
 | a == a' = axiom' ind expr proven (Just 6)
axiom  expr@(b' :->: (a :|: b)) ind proven
 | b == b' = axiom' ind expr proven (Just 7)
axiom  expr@((a :->: c) :->: ((b :->: c') :->: ((a' :|: b') :->: c''))) ind proven
 | a == a' && b == b' && c == c' && c == c'' = axiom' ind expr proven (Just 8)
axiom  expr@((a :->: b) :->: ((a' :->: Neg (b')) :->: Neg (a''))) ind proven
 | a == a' && a == a'' && b == b' = axiom' ind expr proven (Just 9)
axiom  expr@(Neg (Neg (a)) :->: a') ind proven
 | a == a' = axiom' ind expr proven (Just 10)
axiom _ _ _ = Nothing

{- Takes number of line, expression, map of proven expressions and (Maybe numberOfAxiom) and 
returns updated map of proven expressions if numberOfAxiom /= Nothing 
or Nothing otherwise. -}
axiom' :: Int -> Expr -> Map.Map Expr Line -> Maybe Int -> Maybe (Map.Map Expr Line)
axiom' _ _ _ Nothing = Nothing
axiom' ind expr proven (Just num) = Just (Map.insert expr (Axiom ind num) proven)

{- Takes map (Expr -> [(Expr, Int)] and insert given (v, ind) in list by given key -}
getAdd :: Map.Map Expr [(Expr, Int)]-> Expr -> Expr -> Int -> Map.Map Expr [(Expr, Int)]
getAdd map k v ind = case (Map.lookup k map) of
        Just vs -> Map.insert k ((v, ind) : vs) map
        Nothing -> Map.insert k [(v, ind)] map

{- Takes impl, map of impls, index and return updated map of impls -}
addImpl :: Expr -> Map.Map Expr [(Expr, Int)] -> Int -> Map.Map Expr [(Expr, Int)]
addImpl (l :->: r) impls ind = getAdd impls r l ind 
addImpl _ impls _            = impls

{- Takes map of hypots, expession, map of proven expressions and line index and
   returns updated map of proven expressions if given expr is hypot or Nothing otherwise -}
isHypot :: Map.Map Expr Int -> Expr -> Map.Map Expr Line -> Int -> Maybe (Map.Map Expr Line)
isHypot hypots expr proven ind = case (Map.lookup expr hypots) of
    Just x -> Just (Map.insert expr (Hypot ind x) proven)
    Nothing -> Nothing

mp' :: [(Expr, Int)] -> Map.Map Expr Line -> Maybe (Int, Int)
mp' exprs proven = case (filter (\(expr, indx) -> ((Map.lookup expr proven) /= Nothing)) exprs) of
    ((ex, ind) : _) -> case (Map.lookup ex proven) of 
        (Just (MP lineIndex a b)) -> Just (ind, lineIndex)
        (Just (Hypot lineIndex num)) -> Just (ind, lineIndex)
        (Just (Axiom lineIndex num)) -> Just (ind, lineIndex)
    []       -> Nothing

mp :: Map.Map Expr [(Expr, Int)] -> Expr -> Map.Map Expr Line -> Int -> Maybe (Map.Map Expr Line)
mp impls expr proven ind = case (Map.lookup expr impls) of
    Just lefts -> case (mp' lefts proven) of
        (Just (ind1, ind2)) -> Just (Map.insert expr (MP ind ind1 ind2) proven)
        Nothing -> Nothing
    Nothing -> Nothing

{- Takes map of proven expressions, map of hypots, map of impls and returns
   updated map of proven expressions if given expr is hypot/MP/axiom or Nothing otherwise. -}
verifyLine :: Map.Map Expr Line -> Map.Map Expr Int -> Map.Map Expr [(Expr, Int)]  -> Expr -> Int -> Maybe  (Map.Map Expr Line)
verifyLine proven hypots impls expr ind = case (Map.lookup expr proven) of
    (Just _) -> (Just proven) -- Don't update map if this is not first entry of expression
    Nothing -> case (isHypot hypots expr proven ind) of
        (Just proven') -> (Just proven')
        Nothing        -> case (axiom expr ind proven) of
            (Just proven') -> (Just proven')
            Nothing -> mp impls expr proven ind

verifyProof :: Expr -> Map.Map Expr Line -> Map.Map Expr Int -> Map.Map Expr [(Expr, Int)] -> [Expr] -> Int -> (Map.Map Int Expr) -> (Bool, Map.Map Expr Line, Map.Map Expr [(Expr, Int)], Map.Map Int Expr)
verifyProof toProof proven hypots impls (expr : []) ind  ind2Expr = case (toProof == expr) of
    True -> case (verifyLine proven hypots impls expr ind) of 
        (Just proven') -> (True,  proven', impls, (Map.insert ind expr ind2Expr)) 
        Nothing        -> (False, proven, impls, ind2Expr)
    False -> (False, proven, impls, ind2Expr)
verifyProof toProof proven hypots impls (expr : exprs) ind ind2Expr =
    case (verifyLine proven hypots impls expr ind) of 
        (Just proven') -> verifyProof toProof proven' hypots (addImpl expr impls ind) exprs (ind + 1) (Map.insert ind expr ind2Expr)
        Nothing        -> (False, proven, impls, ind2Expr)

{- Returns value stored by key `k` in given map `m` -}
get m k = case (Map.lookup k m) of
    (Just v) -> v
    Nothing  -> error (show k)

{- Returns set of indexes of used in proof lines -}
markUsedLines :: Map.Map Int Expr -> Map.Map Expr Line -> Int -> Set.Set Expr -> Set.Set Expr 
markUsedLines ind2Expr proven ind used = case (proven `get` (ind2Expr `get` ind)) of
    (MP    ind a b) -> (Set.insert (ind2Expr `get` b) (Set.insert (ind2Expr `get` a) ((markUsedLines ind2Expr proven a used) `Set.union` (markUsedLines ind2Expr proven b used))))
    _               -> used