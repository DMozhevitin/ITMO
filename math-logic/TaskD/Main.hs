module Main where
import Lexer
import Parser
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Ord (comparing)

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

main :: IO()
main = interact (unlines . solve)

test2 :: String   -> String
test2 s = show $ findVarNames $ parse $ alexScanTokens s

solve :: String -> [String]
solve s = ans where 
    expr = parse $ alexScanTokens s
    allVars = findVarNames expr
    ans = solve' (findHypots expr) (findHypots2 expr) expr allVars 

solve' Nothing Nothing _ _ = [":("]
solve' (Just (hypots, lines)) _ expr allVars = ans where
    line2Proof = generateProof' expr lines []
    ((minHypotsSet, proof) : []) = mergeProofs expr line2Proof [] allVars Set.empty hypots
    ans = (createProofHead (filterHypots hypots minHypotsSet) expr) : (map show proof)

solve' Nothing (Just (hypots, lines)) expr' allVars = ans where
    expr = (Neg expr') 
    line2Proof = generateProof' expr lines []     
    ((minHypotsSet, proof) : []) = mergeProofs expr line2Proof [] allVars Set.empty hypots
    ans = (createProofHead (filterHypots hypots minHypotsSet) expr) : (map show proof)

listToStr [] s = s
listToStr (x : xs) s = listToStr xs (s ++ show x ++ "\n")

createProofHead hypotNames expr = (intercalate ", " hypotNames) ++ " |- " ++ (show expr)

varName (Neg (Var name)) = name
varName (Var name) = name

hypotName :: (String, Int) -> String
hypotName (name, 0) = "!" ++ name
hypotName (name, 1) = name
 
filterHypots hypotNames line2Expr = map hypotName (filter (\(x, y) -> Set.member x hypotNames) (Map.toList line2Expr))

-- rebuild proof Г, a |- b with Г |- a -> b 
deduction :: [Expr] -> [Expr] -> Expr -> Expr -> ([Expr], Expr)
deduction    hypots    proof     toProof  alpha = do
    let n = length hypots
    let hypotsMap = Map.fromList (zip hypots [1..n])

    case (verifyProof toProof Map.empty hypotsMap Map.empty proof 1 Map.empty) of
        (False, _ ,    _,     _        ) ->  error "Proof is incorrect"
        (True, proven, impls, ind2Expr) -> (rebuildProof alpha proof proven ind2Expr 1 [], alpha :->: toProof)


rebuildProof :: Expr -> [Expr] -> Map.Map Expr Line -> Map.Map Int Expr -> Int -> [Expr] -> [Expr]
rebuildProof alpha []             _      _        _   newProof = newProof
rebuildProof alpha (expr : exprs) proven ind2Expr ind newProof = rebuildProof alpha exprs proven ind2Expr (ind + 1) (newProof ++ newLines) where
    newLines = rebuildLine expr alpha (proven `get` (ind2Expr `get` ind)) ind2Expr
    

rebuildLine :: Expr -> Expr -> Line -> Map.Map Int Expr -> [Expr]
rebuildLine expr alpha (Hypot _ _)     _        = if (expr == alpha) then 
                                                    aImplA expr
                                                else    
                                                    [expr :->: (alpha :->: expr), expr, alpha :->: expr]
rebuildLine expr alpha (Axiom _ _)     _        = [expr :->: (alpha :->: expr), expr, alpha :->: expr]
rebuildLine expr alpha (MP _ aImplB a) ind2Expr = [(alpha :->: aExpr) :->: ((alpha :->: aImplBExpr) :->: (alpha :->: expr)), (alpha :->: aImplBExpr) :->: (alpha :->: expr), alpha :->: expr] where
    aExpr = ind2Expr `get` a
    aImplBExpr = ind2Expr `get` aImplB


aImplA a = [e1, e2, e1 :->: (e2 :->: (a :->: a)), e2 :->: (a :->: a), a :->: a] where
    e1 = a :->: (a :->: a)
    e2 = a :->: ((a :->: a) :->: a)

boolValue 1 = True
boolValue 0 = False

evaluate :: Expr -> Map.Map String Int -> Bool
evaluate (a :->: b) vars   =  (not $ evaluate a vars) || evaluate b vars
evaluate (a :|: b)  vars   =  evaluate a vars || evaluate b vars
evaluate (a :&: b)  vars   =  evaluate a vars && evaluate b vars
evaluate (Neg a)    vars   =  not $ evaluate a vars
evaluate (Var a)    vars   =  boolValue $ vars `get` a      


createExprValuesMap :: Expr -> Map.Map String Int -> (Map.Map Expr Bool, Bool)
createExprValuesMap (a :->: b)  vars   =  (Map.insert (a :->: b) ((not valLeft) || valRight) (valsRight `Map.union` valsLeft), (not valLeft) || valRight) where
    (valsLeft, valLeft) = createExprValuesMap a vars
    (valsRight, valRight) = createExprValuesMap b vars
createExprValuesMap (a :|: b)  vars   =  (Map.insert (a :|: b) (valLeft || valRight) (valsRight `Map.union` valsLeft), valLeft || valRight) where
    (valsLeft, valLeft) = createExprValuesMap a vars
    (valsRight, valRight) = createExprValuesMap b vars
createExprValuesMap (a :&: b)  vars   =  (Map.insert (a :&: b) (valLeft && valRight) (valsRight `Map.union` valsLeft), valLeft && valRight) where
    (valsLeft, valLeft) = createExprValuesMap a vars
    (valsRight, valRight) = createExprValuesMap b vars
createExprValuesMap (Neg a)    vars   =  (Map.insert (Neg a) (not val) vals', not val) where
    (vals', val) = createExprValuesMap a vars
createExprValuesMap (Var a)    vars   =  (Map.singleton (Var a) (boolValue (vars `get` a)), boolValue $ vars `get` a) 

findVarNames :: Expr ->  Set.Set String
findVarNames    (a :->: b) = findVarNames a `Set.union` findVarNames b 
findVarNames    (a :|: b)  = findVarNames a `Set.union` findVarNames b
findVarNames    (a :&: b)  = findVarNames a `Set.union` findVarNames b
findVarNames    (Neg a)    = findVarNames a
findVarNames    (Var a)    = Set.singleton a

{- replaces meta-variables in proof with given expressions -}
makeSubstitution :: Map.Map String Expr -> Expr -> Expr
makeSubstitution    vars (a :->: b)  = (makeSubstitution vars a) :->: (makeSubstitution vars b)
makeSubstitution    vars (a :|: b)   = (makeSubstitution vars a) :|: (makeSubstitution vars b)
makeSubstitution    vars (a :&: b)   = (makeSubstitution vars a) :&: (makeSubstitution vars b)
makeSubstitution    vars (Neg a)     = Neg (makeSubstitution vars a)
makeSubstitution    vars (Var a)     = vars `get` a

generateProof :: Expr -> Map.Map String Int -> Map.Map Expr Bool -> [Expr]
generateProof (a :->: b) hypots exprVals = proofLeft ++ proofRight ++ proofImpl where
    proofLeft  = generateProof a hypots exprVals
    proofRight = generateProof b hypots exprVals
    proofImpl = map (makeSubstitution (Map.fromList [("A", a), ("B", b)])) (case (exprVals `get` a, exprVals `get` b) of --TODO: передать мапу expr -> bool чтобы не делать evaluate
        (False, False) -> impl00
        (False, True) -> impl01
        (True, False) -> impl10
        (True, True) -> impl11)

generateProof (a :&: b) hypots exprVals = proofLeft ++ proofRight ++ proofAnd where
    proofLeft  = generateProof a hypots exprVals
    proofRight = generateProof b hypots exprVals
    proofAnd = map (makeSubstitution (Map.fromList [("A", a), ("B", b)])) (case (exprVals `get` a, exprVals `get` b) of --TODO: передать мапу expr -> bool чтобы не делать evaluate
        (False, False) -> and00
        (False, True) -> and01
        (True, False) -> and10
        (True, True) -> and11)

generateProof (a :|: b) hypots exprVals = proofLeft ++ proofRight ++ proofOr where
    proofLeft  = generateProof a hypots exprVals
    proofRight = generateProof b hypots exprVals
    proofOr = map (makeSubstitution (Map.fromList [("A", a), ("B", b)])) (case (exprVals `get` a, exprVals `get` b) of --TODO: передать мапу expr -> bool чтобы не делать evaluate
        (False, False) -> or00
        (False, True)  -> or01
        (True, False)  -> or10
        (True, True)   -> or11)    

generateProof (Neg a) hypots exprVals = p ++ proofNeg where
    p = generateProof a hypots exprVals
    proofNeg = map (makeSubstitution (Map.fromList [("A", a)])) (case (exprVals `get` a) of --TODO: передать мапу expr -> bool чтобы не делать evaluate
        False -> not0
        True  -> not1)

generateProof (Var a) hypots exprVals = case (hypots `get` a) of
    1 -> [(Var a)]
    0 -> [(Neg (Var a))]        


mergeProofs :: Expr -> [(Map.Map String Int, [Expr])] -> [(Map.Map String Int, [Expr])] -> Set.Set String -> Set.Set String -> Set.Set String -> [(Map.Map String Int, [Expr])]
mergeProofs    expr (x : [])   []          _       _        _      = (x : [])
mergeProofs    expr []         (x : [])    _       _        _      = (x : [])
mergeProofs    expr []         line2Proof' allVars deletedVars hypots = (mergeProofs expr line2Proof' [] allVars Set.empty hypots)                
mergeProofs    expr line2Proof line2Proof' allVars deletedVars hypots = res where 
    usedVars     = getUsedVars (map fst line2Proof) Set.empty
    unusedVars   = allVars `Set.difference` usedVars
    varsToDelete = Set.toList (((allVars `Set.difference` unusedVars) `Set.difference` hypots) `Set.difference` deletedVars)
    varToDelete = (head varsToDelete)
    (minus2Lines, mergedLine) = merge2Lines line2Proof varToDelete expr
    res = if (varsToDelete == []) then 
                                line2Proof
                            else 
                                mergeProofs expr minus2Lines (mergedLine : line2Proof') allVars (allVars `Set.difference` (Set.singleton varToDelete)) hypots

getUsedVars :: [(Map.Map String Int)] -> Set.Set String -> Set.Set String
getUsedVars    []             vars = vars
getUsedVars    (line : lines) vars = getUsedVars lines (vars `Set.union` (Set.fromList $ Map.keys line)) 

filterList [] _ _ _ res = res
filterList (x : xs) i j ind res = if (ind == i || ind == j) then
        filterList xs i j (ind + 1) res
    else
        x : (filterList xs i j (ind + 1) res)

merge2Lines :: [(Map.Map String Int, [Expr])] -> String -> Expr -> ([(Map.Map String Int, [Expr])], (Map.Map String Int, [Expr]))
merge2Lines line2Proof var expr = (merge2Lines' line2Proof var expr indexes line2Proof') where
    line2Proof' = map (\(x, y) -> (Map.delete var x, y)) line2Proof
    indexes = case (findDuplicates (map fst line2Proof') 0) of
        (Just is) -> is
        Nothing -> error (show var)

merge2Lines' line2Proof var expr (ind1, ind2) line2Proof' = (filterList line2Proof ind1 ind2 0 [], (fst $ line2Proof' !! ind1, proofWithoutHypot)) where
    (line1, proof1) = line2Proof !! ind1
    (line2, proof2) = line2Proof !! ind2
    alpha1 = case (line1 `get` var) of 
        1 -> (Var var)
        0 -> (Neg (Var var))
    alpha2 = case (line2 `get` var) of 
        1 -> (Var var)
        0 -> (Neg (Var var))
    hypots1 = (map mark2Expression (Map.toList line1)) ++ [alpha1]
    hypots2 = (map mark2Expression (Map.toList line2)) ++ [alpha2]    
    (proof1', expr1') = deduction hypots1 proof1 expr alpha1
    (proof2', expr2') = deduction hypots2 proof2 expr alpha2
    proof' = proof1' ++ proof2'
    proofWithoutHypot = proof' ++ (map (makeSubstitution (Map.fromList [("A", expr), ("B", (Var var))])) hypotExclusion)

mark2Expression :: (String, Int) -> Expr
mark2Expression (name, 0) = (Neg (Var name))
mark2Expression (name, 1) = (Var name)

findDuplicates :: Eq a => [a] -> Int -> Maybe (Int, Int)
findDuplicates [] _ = Nothing
findDuplicates (x : xs) ind = case (findDuplicate x (ind + 1) xs) of
    Just ind2 -> Just (ind, ind2)
    Nothing -> findDuplicates xs (ind + 1)

findDuplicate :: Eq a => a -> Int -> [a] -> Maybe Int
findDuplicate elem _ [] = Nothing
findDuplicate elem ind (x : xs) = case (elem == x) of
    True -> Just ind
    False -> findDuplicate elem (ind + 1) xs


generateProof' :: Expr -> [Map.Map String Int] -> [(Map.Map String Int, [Expr])] -> [(Map.Map String Int, [Expr])]
generateProof' _    []              res = res  
generateProof' expr (line : lines)  res = (generateProof' expr lines ((line, (generateProof expr line (fst (createExprValuesMap expr line)))) : res)) 


findHypots :: Expr -> Maybe (Set.Set String, [Map.Map String Int])
findHypots expr = case (findHypots' expr) of
    Nothing -> Nothing
    (Just (hyps, table)) -> Just (hyps, fromJust $ checkHypotsSet expr table hyps)

findHypots' expr = hypots  where
    vars     = findVarNames expr
    varsPset = sortBy (comparing Set.size) (Set.toList (Set.powerSet vars))
    table    = map Map.fromList (createTruthTable (Set.toList vars))
    hypots = case (dropWhile (isNothing . checkHypotsSet expr table) varsPset) of
        [] -> Nothing
        xs -> Just (head xs, table)

findHypots2 :: Expr -> Maybe (Set.Set String, [Map.Map String Int])
findHypots2 expr = case (findHypots2' expr) of
    Nothing -> Nothing
    (Just (hyps, table)) -> Just (hyps, fromJust $ checkHypotsSet2 expr table hyps)

findHypots2' expr = hypots  where
            vars     = findVarNames expr
            varsPset = sortBy (comparing Set.size) (Set.toList (Set.powerSet vars))
            table    = map Map.fromList (createTruthTable (Set.toList vars))
            hypots   = case (dropWhile (isNothing . checkHypotsSet2 expr table) varsPset) of
                [] -> Nothing
                xs -> Just (head xs, table)

{- Checks that expression is True when given variables are True and returns all lines
   from the truth table where these variables are True -}
checkHypotsSet :: Expr -> [Map.Map String Int] -> Set.Set String -> Maybe [Map.Map String Int]
checkHypotsSet expr table hypots       = if (all (== True) fValues) then
        Just table' 
    else 
        Nothing where
    table' = if (null hypots) then
            table
        else
            filter (allTrue (Set.toList hypots)) table
    fValues = map (evaluate expr) table'

checkHypotsSet2 :: Expr -> [Map.Map String Int] -> Set.Set String -> Maybe [Map.Map String Int]
checkHypotsSet2 expr table hypots       = if (all (== False) fValues) then
        Just table' 
    else 
        Nothing where
    table' = if (null hypots) then
            table
        else
            filter (allFalse (Set.toList hypots)) table
    fValues = map (evaluate expr) table'    

allTrue  = all' 1
allFalse = all' 0

all' val [] _ = True
all' val (x : xs) map = if (map `get` x == val) then
                            all' val xs map 
                        else
                            False

createTruthTable vars = map (\lst -> zip vars lst) binLists where
    n = (2 ^ length vars) - 1
    binLists = binaryLists n (length vars) []

binaryLists 0 sz bins = (replicate sz 0) : bins
binaryLists n sz bins = (addLeadingZeros sz $ toBinaryList n) : (binaryLists (n - 1) sz bins) 

addLeadingZeros sz lst = replicate (sz - length lst) 0 ++ lst

toBinaryList 0 = [0]
toBinaryList n = reverse $ toBinaryList' n

toBinaryList' 0 = []
toBinaryList' n = (n `mod` 2) : toBinaryList' (n `div` 2)     

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

{- Returns value stored by key `k` in given map `m` -}
get m k = case (Map.lookup k m) of
    (Just v) -> v
    Nothing  -> error (show k)



-------------------------------------------------------------------------------------------------------
-- base proofs

and00 = map (parseProof . alexScanTokens) [
    "((A)&(B))->(A)",
    "!(A)",
    "!(A)->(((A)&(B))->!(A))",
    "((A)&(B))->!(A)",
    "(((A)&(B))->(A))->((((A)&(B))->!(A))->!((A)&(B)))",
    "(((A)&(B))->!(A))->!((A)&(B))",
    "!((A)&(B))"]

and01 = map (parseProof . alexScanTokens) [
    "((A)&(B))->(A)",
    "!(A)->(((A)&(B))->!(A))",
    "!(A)",
    "((A)&(B))->!(A)",
    "(((A)&(B))->(A))->((((A)&(B))->!(A))->!((A)&(B)))",
    "(((A)&(B))->!(A))->!((A)&(B))",
    "!((A)&(B))"]

and10 = map (parseProof . alexScanTokens) [
    "((A)&(B))->(B)",
    "!(B)",
    "!(B)->(((A)&(B))->!(B))",
    "((A)&(B))->!(B)",
    "(((A)&(B))->(B))->((((A)&(B))->!(B))->!((A)&(B)))",
    "(((A)&(B))->!(B))->!((A)&(B))",
    "!((A)&(B))"]

and11 = map (parseProof . alexScanTokens) [
    "(A)",
    "(B)",
    "(A)->((B)->((A)&(B)))",
    "(B)->((A)&(B))",
    "(A)&(B)"]

or00 = map (parseProof . alexScanTokens) [
    "!(A)",
    "!(B)",
    "(((A)|(B))->(A))->((((A)|(B))->!(A))->!((A)|(B)))",
    "!(A)->(((A)|(B))->!(A))",
    "((A)|(B))->!(A)",
    "!(A)",
    "(!(A)->(((A)|(B))->!(A)))",
    "(((A)|(B))->!(A))",
    "!(B)",
    "(!(B)->(((A)|(B))->!(B)))",
    "(((A)|(B))->!(B))",
    "(((A)|(B))->(((A)|(B))->((A)|(B))))",
    "(((A)|(B))->((((A)|(B))->((A)|(B)))->((A)|(B))))",
    "((((A)|(B))->(((A)|(B))->((A)|(B))))->((((A)|(B))->((((A)|(B))->((A)|(B)))->((A)|(B))))->(((A)|(B))->((A)|(B)))))",
    "((((A)|(B))->((((A)|(B))->((A)|(B)))->((A)|(B))))->(((A)|(B))->((A)|(B))))",
    "(((A)|(B))->((A)|(B)))",
    "((A)->((A)->(A)))",
    "(((A)->((A)->(A)))->(((A)|(B))->((A)->((A)->(A)))))",
    "(((A)|(B))->((A)->((A)->(A))))",
    "(((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A))))",
    "((((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A))))->(((A)|(B))->(((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A))))))",
    "(((A)|(B))->(((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A)))))",
    "((A)->(((A)->(A))->(A)))",
    "(((A)->(((A)->(A))->(A)))->(((A)|(B))->((A)->(((A)->(A))->(A)))))",
    "(((A)|(B))->((A)->(((A)->(A))->(A))))",
    "((((A)|(B))->((A)->((A)->(A))))->((((A)|(B))->(((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A)))))->(((A)|(B))->(((A)->(((A)->(A))->(A)))->((A)->(A))))))",
    "((((A)|(B))->(((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A)))))->(((A)|(B))->(((A)->(((A)->(A))->(A)))->((A)->(A)))))",
    "(((A)|(B))->(((A)->(((A)->(A))->(A)))->((A)->(A))))",
    "((((A)|(B))->((A)->(((A)->(A))->(A))))->((((A)|(B))->(((A)->(((A)->(A))->(A)))->((A)->(A))))->(((A)|(B))->((A)->(A)))))",
    "((((A)|(B))->(((A)->(((A)->(A))->(A)))->((A)->(A))))->(((A)|(B))->((A)->(A))))",
    "(((A)|(B))->((A)->(A)))",
    "!(B)",
    "(!(B)->(((A)|(B))->!(B)))",
    "(((A)|(B))->!(B))",
    "(!(B)->((B)->!(B)))",
    "((!(B)->((B)->!(B)))->(((A)|(B))->(!(B)->((B)->!(B)))))",
    "(((A)|(B))->(!(B)->((B)->!(B))))",
    "((((A)|(B))->!(B))->((((A)|(B))->(!(B)->((B)->!(B))))->(((A)|(B))->((B)->!(B)))))",
    "((((A)|(B))->(!(B)->((B)->!(B))))->(((A)|(B))->((B)->!(B))))",
    "(((A)|(B))->((B)->!(B)))",
    "!(A)",
    "(!(A)->(((A)|(B))->!(A)))",
    "(((A)|(B))->!(A))",
    "(!(A)->((B)->!(A)))",
    "((!(A)->((B)->!(A)))->(((A)|(B))->(!(A)->((B)->!(A)))))",
    "(((A)|(B))->(!(A)->((B)->!(A))))",
    "((((A)|(B))->!(A))->((((A)|(B))->(!(A)->((B)->!(A))))->(((A)|(B))->((B)->!(A)))))",
    "((((A)|(B))->(!(A)->((B)->!(A))))->(((A)|(B))->((B)->!(A))))",
    "(((A)|(B))->((B)->!(A)))",
    "((B)->((B)->(B)))",
    "(((B)->((B)->(B)))->(((A)|(B))->((B)->((B)->(B)))))",
    "(((A)|(B))->((B)->((B)->(B))))",
    "((B)->(((B)->(B))->(B)))",
    "(((B)->(((B)->(B))->(B)))->(((A)|(B))->((B)->(((B)->(B))->(B)))))",
    "(((A)|(B))->((B)->(((B)->(B))->(B))))",
    "(((B)->((B)->(B)))->(((B)->(((B)->(B))->(B)))->((B)->(B))))",
    "((((B)->((B)->(B)))->(((B)->(((B)->(B))->(B)))->((B)->(B))))->(((A)|(B))->(((B)->((B)->(B)))->(((B)->(((B)->(B))->(B)))->((B)->(B))))))",
    "(((A)|(B))->(((B)->((B)->(B)))->(((B)->(((B)->(B))->(B)))->((B)->(B)))))",
    "((((A)|(B))->((B)->((B)->(B))))->((((A)|(B))->(((B)->((B)->(B)))->(((B)->(((B)->(B))->(B)))->((B)->(B)))))->(((A)|(B))->(((B)->(((B)->(B))->(B)))->((B)->(B))))))",
    "((((A)|(B))->(((B)->((B)->(B)))->(((B)->(((B)->(B))->(B)))->((B)->(B)))))->(((A)|(B))->(((B)->(((B)->(B))->(B)))->((B)->(B)))))",
    "(((A)|(B))->(((B)->(((B)->(B))->(B)))->((B)->(B))))",
    "((((A)|(B))->((B)->(((B)->(B))->(B))))->((((A)|(B))->(((B)->(((B)->(B))->(B)))->((B)->(B))))->(((A)|(B))->((B)->(B)))))",
    "((((A)|(B))->(((B)->(((B)->(B))->(B)))->((B)->(B))))->(((A)|(B))->((B)->(B))))",
    "(((A)|(B))->((B)->(B)))",
    "((!(A)->(B))->((!(A)->!(B))->!!(A)))",
    "(((!(A)->(B))->((!(A)->!(B))->!!(A)))->(((A)|(B))->((!(A)->(B))->((!(A)->!(B))->!!(A)))))",
    "(((A)|(B))->((!(A)->(B))->((!(A)->!(B))->!!(A))))",
    "(((!(A)->(B))->((!(A)->!(B))->!!(A)))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A)))))",
    "((((!(A)->(B))->((!(A)->!(B))->!!(A)))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A)))))->(((A)|(B))->(((!(A)->(B))->((!(A)->!(B))->!!(A)))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A)))))))",
    "(((A)|(B))->(((!(A)->(B))->((!(A)->!(B))->!!(A)))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))))",
    "((((A)|(B))->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((((A)|(B))->(((!(A)->(B))->((!(A)->!(B))->!!(A)))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))))->(((A)|(B))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A)))))))",
    "((((A)|(B))->(((!(A)->(B))->((!(A)->!(B))->!!(A)))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))))->(((A)|(B))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))))",
    "(((A)|(B))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A)))))",
    "((B)->(!(A)->(B)))",
    "(((B)->(!(A)->(B)))->(((A)|(B))->((B)->(!(A)->(B)))))",
    "(((A)|(B))->((B)->(!(A)->(B))))",
    "(((B)->(!(A)->(B)))->((B)->((B)->(!(A)->(B)))))",
    "((((B)->(!(A)->(B)))->((B)->((B)->(!(A)->(B)))))->(((A)|(B))->(((B)->(!(A)->(B)))->((B)->((B)->(!(A)->(B)))))))",
    "(((A)|(B))->(((B)->(!(A)->(B)))->((B)->((B)->(!(A)->(B))))))",
    "((((A)|(B))->((B)->(!(A)->(B))))->((((A)|(B))->(((B)->(!(A)->(B)))->((B)->((B)->(!(A)->(B))))))->(((A)|(B))->((B)->((B)->(!(A)->(B)))))))",
    "((((A)|(B))->(((B)->(!(A)->(B)))->((B)->((B)->(!(A)->(B))))))->(((A)|(B))->((B)->((B)->(!(A)->(B))))))",
    "(((A)|(B))->((B)->((B)->(!(A)->(B)))))",
    "(((B)->(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B)))))",
    "((((B)->(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B)))))->(((A)|(B))->(((B)->(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B)))))))",
    "(((A)|(B))->(((B)->(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B))))))",
    "((((A)|(B))->((B)->(B)))->((((A)|(B))->(((B)->(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B))))))->(((A)|(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B)))))))",
    "((((A)|(B))->(((B)->(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B))))))->(((A)|(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B))))))",
    "(((A)|(B))->(((B)->((B)->(!(A)->(B))))->((B)->(!(A)->(B)))))",
    "((B)->(!(A)->(B)))",
    "(((B)->(!(A)->(B)))->(((A)|(B))->((B)->(!(A)->(B)))))",
    "(((A)|(B))->((B)->(!(A)->(B))))",
    "(!(B)->(!(A)->!(B)))",
    "((!(B)->(!(A)->!(B)))->(((A)|(B))->(!(B)->(!(A)->!(B)))))",
    "(((A)|(B))->(!(B)->(!(A)->!(B))))",
    "((!(B)->(!(A)->!(B)))->((B)->(!(B)->(!(A)->!(B)))))",
    "(((!(B)->(!(A)->!(B)))->((B)->(!(B)->(!(A)->!(B)))))->(((A)|(B))->((!(B)->(!(A)->!(B)))->((B)->(!(B)->(!(A)->!(B)))))))",
    "(((A)|(B))->((!(B)->(!(A)->!(B)))->((B)->(!(B)->(!(A)->!(B))))))",
    "((((A)|(B))->(!(B)->(!(A)->!(B))))->((((A)|(B))->((!(B)->(!(A)->!(B)))->((B)->(!(B)->(!(A)->!(B))))))->(((A)|(B))->((B)->(!(B)->(!(A)->!(B)))))))",
    "((((A)|(B))->((!(B)->(!(A)->!(B)))->((B)->(!(B)->(!(A)->!(B))))))->(((A)|(B))->((B)->(!(B)->(!(A)->!(B))))))",
    "(((A)|(B))->((B)->(!(B)->(!(A)->!(B)))))",
    "(((B)->!(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))",
    "((((B)->!(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))->(((A)|(B))->(((B)->!(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))))",
    "(((A)|(B))->(((B)->!(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B))))))",
    "((((A)|(B))->((B)->!(B)))->((((A)|(B))->(((B)->!(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B))))))->(((A)|(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))))",
    "((((A)|(B))->(((B)->!(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B))))))->(((A)|(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B))))))",
    "(((A)|(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))",
    "((((A)|(B))->((B)->(!(B)->(!(A)->!(B)))))->((((A)|(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))->(((A)|(B))->((B)->(!(A)->!(B))))))",
    "((((A)|(B))->(((B)->(!(B)->(!(A)->!(B))))->((B)->(!(A)->!(B)))))->(((A)|(B))->((B)->(!(A)->!(B)))))",
    "(((A)|(B))->((B)->(!(A)->!(B))))",
    "(((B)->(!(A)->(B)))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))",
    "((((B)->(!(A)->(B)))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))->(((A)|(B))->(((B)->(!(A)->(B)))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))))",
    "(((A)|(B))->(((B)->(!(A)->(B)))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A))))))",
    "((((A)|(B))->((B)->(!(A)->(B))))->((((A)|(B))->(((B)->(!(A)->(B)))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A))))))->(((A)|(B))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))))",
    "((((A)|(B))->(((B)->(!(A)->(B)))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A))))))->(((A)|(B))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A))))))",
    "(((A)|(B))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))",
    "((((A)|(B))->((B)->((!(A)->(B))->((!(A)->!(B))->!!(A)))))->((((A)|(B))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))->(((A)|(B))->((B)->((!(A)->!(B))->!!(A))))))",
    "((((A)|(B))->(((B)->((!(A)->(B))->((!(A)->!(B))->!!(A))))->((B)->((!(A)->!(B))->!!(A)))))->(((A)|(B))->((B)->((!(A)->!(B))->!!(A)))))",
    "(((A)|(B))->((B)->((!(A)->!(B))->!!(A))))",
    "(((B)->(!(A)->!(B)))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))",
    "((((B)->(!(A)->!(B)))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))->(((A)|(B))->(((B)->(!(A)->!(B)))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))))",
    "(((A)|(B))->(((B)->(!(A)->!(B)))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A)))))",
    "((((A)|(B))->((B)->(!(A)->!(B))))->((((A)|(B))->(((B)->(!(A)->!(B)))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A)))))->(((A)|(B))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))))",
    "((((A)|(B))->(((B)->(!(A)->!(B)))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A)))))->(((A)|(B))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A)))))",
    "(((A)|(B))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))",
    "((((A)|(B))->((B)->((!(A)->!(B))->!!(A))))->((((A)|(B))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))->(((A)|(B))->((B)->!!(A)))))",
    "((((A)|(B))->(((B)->((!(A)->!(B))->!!(A)))->((B)->!!(A))))->(((A)|(B))->((B)->!!(A))))",
    "(((A)|(B))->((B)->!!(A)))",
    "(!!(A)->(A))",
    "((!!(A)->(A))->(((A)|(B))->(!!(A)->(A))))",
    "(((A)|(B))->(!!(A)->(A)))",
    "((!!(A)->(A))->((B)->(!!(A)->(A))))",
    "(((!!(A)->(A))->((B)->(!!(A)->(A))))->(((A)|(B))->((!!(A)->(A))->((B)->(!!(A)->(A))))))",
    "(((A)|(B))->((!!(A)->(A))->((B)->(!!(A)->(A)))))",
    "((((A)|(B))->(!!(A)->(A)))->((((A)|(B))->((!!(A)->(A))->((B)->(!!(A)->(A)))))->(((A)|(B))->((B)->(!!(A)->(A))))))",
    "((((A)|(B))->((!!(A)->(A))->((B)->(!!(A)->(A)))))->(((A)|(B))->((B)->(!!(A)->(A)))))",
    "(((A)|(B))->((B)->(!!(A)->(A))))",
    "(((B)->!!(A))->(((B)->(!!(A)->(A)))->((B)->(A))))",
    "((((B)->!!(A))->(((B)->(!!(A)->(A)))->((B)->(A))))->(((A)|(B))->(((B)->!!(A))->(((B)->(!!(A)->(A)))->((B)->(A))))))",
    "(((A)|(B))->(((B)->!!(A))->(((B)->(!!(A)->(A)))->((B)->(A)))))",
    "((((A)|(B))->((B)->!!(A)))->((((A)|(B))->(((B)->!!(A))->(((B)->(!!(A)->(A)))->((B)->(A)))))->(((A)|(B))->(((B)->(!!(A)->(A)))->((B)->(A))))))",
    "((((A)|(B))->(((B)->!!(A))->(((B)->(!!(A)->(A)))->((B)->(A)))))->(((A)|(B))->(((B)->(!!(A)->(A)))->((B)->(A)))))",
    "(((A)|(B))->(((B)->(!!(A)->(A)))->((B)->(A))))",
    "((((A)|(B))->((B)->(!!(A)->(A))))->((((A)|(B))->(((B)->(!!(A)->(A)))->((B)->(A))))->(((A)|(B))->((B)->(A)))))",
    "((((A)|(B))->(((B)->(!!(A)->(A)))->((B)->(A))))->(((A)|(B))->((B)->(A))))",
    "(((A)|(B))->((B)->(A)))",
    "(((A)->(A))->(((B)->(A))->(((A)|(B))->(A))))",
    "((((A)->(A))->(((B)->(A))->(((A)|(B))->(A))))->(((A)|(B))->(((A)->(A))->(((B)->(A))->(((A)|(B))->(A))))))",
    "(((A)|(B))->(((A)->(A))->(((B)->(A))->(((A)|(B))->(A)))))",
    "((((A)|(B))->((A)->(A)))->((((A)|(B))->(((A)->(A))->(((B)->(A))->(((A)|(B))->(A)))))->(((A)|(B))->(((B)->(A))->(((A)|(B))->(A))))))",
    "((((A)|(B))->(((A)->(A))->(((B)->(A))->(((A)|(B))->(A)))))->(((A)|(B))->(((B)->(A))->(((A)|(B))->(A)))))",
    "(((A)|(B))->(((B)->(A))->(((A)|(B))->(A))))",
    "((((A)|(B))->((B)->(A)))->((((A)|(B))->(((B)->(A))->(((A)|(B))->(A))))->(((A)|(B))->(((A)|(B))->(A)))))",
    "((((A)|(B))->(((B)->(A))->(((A)|(B))->(A))))->(((A)|(B))->(((A)|(B))->(A))))",
    "(((A)|(B))->(((A)|(B))->(A)))",
    "((((A)|(B))->((A)|(B)))->((((A)|(B))->(((A)|(B))->(A)))->(((A)|(B))->(A))))",
    "((((A)|(B))->(((A)|(B))->(A)))->(((A)|(B))->(A)))",
    "(((A)|(B))->(A))",
    "(((A)|(B))->!(A))->!((A)|(B))",
    "!((A)|(B))"]

or01 = map (parseProof . alexScanTokens) [
    "(B)",
    "(B)->((A)|(B))",
    "(A)|(B)"]

or10 = map (parseProof . alexScanTokens) [
    "(A)",
    "(A)->((A)|(B))",
    "(A)|(B)"]

or11 = map (parseProof . alexScanTokens) [
    "(A)",
    "(A)->((A)|(B))",
    "(A)|(B)"]

impl00 = map (parseProof . alexScanTokens) [
    "!(A)",
    "(!(A)->((A)->!(A)))",
    "((A)->!(A))",
    "!(B)",
    "(!(B)->((A)->!(B)))",
    "((A)->!(B))",
    "((A)->((A)->(A)))",
    "((A)->(((A)->(A))->(A)))",
    "(((A)->((A)->(A)))->(((A)->(((A)->(A))->(A)))->((A)->(A))))",
    "(((A)->(((A)->(A))->(A)))->((A)->(A)))",
    "((A)->(A))",
    "((!(B)->(A))->((!(B)->!(A))->!!(B)))",
    "(((!(B)->(A))->((!(B)->!(A))->!!(B)))->((A)->((!(B)->(A))->((!(B)->!(A))->!!(B)))))",
    "((A)->((!(B)->(A))->((!(B)->!(A))->!!(B))))",
    "((A)->(!(B)->(A)))",
    "(((A)->(!(B)->(A)))->((A)->((A)->(!(B)->(A)))))",
    "((A)->((A)->(!(B)->(A))))",
    "(((A)->(A))->(((A)->((A)->(!(B)->(A))))->((A)->(!(B)->(A)))))",
    "(((A)->((A)->(!(B)->(A))))->((A)->(!(B)->(A))))",
    "((A)->(!(B)->(A)))",
    "(!(A)->(!(B)->!(A)))",
    "((!(A)->(!(B)->!(A)))->((A)->(!(A)->(!(B)->!(A)))))",
    "((A)->(!(A)->(!(B)->!(A))))",
    "(((A)->!(A))->(((A)->(!(A)->(!(B)->!(A))))->((A)->(!(B)->!(A)))))",
    "(((A)->(!(A)->(!(B)->!(A))))->((A)->(!(B)->!(A))))",
    "((A)->(!(B)->!(A)))",
    "(((A)->(!(B)->(A)))->(((A)->((!(B)->(A))->((!(B)->!(A))->!!(B))))->((A)->((!(B)->!(A))->!!(B)))))",
    "(((A)->((!(B)->(A))->((!(B)->!(A))->!!(B))))->((A)->((!(B)->!(A))->!!(B))))",
    "((A)->((!(B)->!(A))->!!(B)))",
    "(((A)->(!(B)->!(A)))->(((A)->((!(B)->!(A))->!!(B)))->((A)->!!(B))))",
    "(((A)->((!(B)->!(A))->!!(B)))->((A)->!!(B)))",
    "((A)->!!(B))",
    "(!!(B)->(B))",
    "((!!(B)->(B))->((A)->(!!(B)->(B))))",
    "((A)->(!!(B)->(B)))",
    "(((A)->!!(B))->(((A)->(!!(B)->(B)))->((A)->(B))))",
    "(((A)->(!!(B)->(B)))->((A)->(B)))",
    "((A)->(B))"]

impl01 = map (parseProof . alexScanTokens) [
    "(B)->((A)->(B))",
    "(B)",
    "(A)->(B)"]

impl10 = map (parseProof . alexScanTokens) [
    "(A)",
    "!(B)",
    "!(B)->(((A)->(B))->!(B))",
    "((A)->(B))->!(B)",
    "((A)->(((A)->(B))->(A)))",
    "(((A)->(B))->(A))",
    "(((A)->(B))->(((A)->(B))->((A)->(B))))",
    "((((A)->(B))->(((A)->(B))->((A)->(B))))->((((A)->(B))->((((A)->(B))->((A)->(B)))->((A)->(B))))->(((A)->(B))->((A)->(B)))))",
    "((((A)->(B))->((((A)->(B))->((A)->(B)))->((A)->(B))))->(((A)->(B))->((A)->(B))))",
    "(((A)->(B))->((((A)->(B))->((A)->(B)))->((A)->(B))))",
    "(((A)->(B))->((A)->(B)))",
    "((((A)->(B))->(A))->((((A)->(B))->((A)->(B)))->(((A)->(B))->(B))))",
    "((((A)->(B))->((A)->(B)))->(((A)->(B))->(B)))",
    "(((A)->(B))->(B))",
    "(((A)->(B))->(B))->((((A)->(B))->!(B))->!((A)->(B)))",
    "(((A)->(B))->!(B))->!((A)->(B))",
    "!((A)->(B))"]

impl11 = map (parseProof . alexScanTokens) [
    "(B)->((A)->(B))",
    "(B)",
    "(A)->(B)"]

not0 = map (parseProof . alexScanTokens) ["!(A)"]    

not1 = map (parseProof . alexScanTokens) [
    "(A)",
    "(A)->(!!!(A)->(A))",
    "!!!(A)->(A)",
    "!!!(A)->!(A)",
    "(!!!(A)->(A))->((!!!(A)->!(A))->!!!!(A))",
    "(!!!(A)->!(A))->!!!!(A)",
    "!!!!(A)",
    "!!!!(A)->!!(A)",
    "!!(A)"]

hypotExclusion = map (parseProof . alexScanTokens) [
    "(B)->((B)|!(B))",
    "(!((B)|!(B))->((B)->!((B)|!(B))))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B)))",
    "((!((B)|!(B))->((B)->!((B)|!(B))))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B))))->(((B)->((B)|!(B)))->((!((B)|!(B))->((B)->!((B)|!(B))))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B)))))",
    "((B)->((B)|!(B)))->((!((B)|!(B))->((B)->!((B)|!(B))))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B))))",
    "!((B)|!(B))->((B)->!((B)|!(B)))",
    "(!((B)|!(B))->((B)->!((B)|!(B))))->(((B)->((B)|!(B)))->(!((B)|!(B))->((B)->!((B)|!(B)))))",
    "((B)->((B)|!(B)))->(!((B)|!(B))->((B)->!((B)|!(B))))",
    "(((B)->((B)|!(B)))->(!((B)|!(B))->((B)->!((B)|!(B)))))->((((B)->((B)|!(B)))->((!((B)|!(B))->((B)->!((B)|!(B))))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B)))))->(((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B)))))",
    "(((B)->((B)|!(B)))->((!((B)|!(B))->((B)->!((B)|!(B))))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B)))))->(((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B))))",
    "((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B)))",
    "(!((B)|!(B))->((B)->((B)|!(B))))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))",
    "((!((B)|!(B))->((B)->((B)|!(B))))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))))->(((B)->((B)|!(B)))->((!((B)|!(B))->((B)->((B)|!(B))))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))))",
    "((B)->((B)|!(B)))->((!((B)|!(B))->((B)->((B)|!(B))))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))))",
    "((B)->((B)|!(B)))->(!((B)|!(B))->((B)->((B)|!(B))))",
    "(((B)->((B)|!(B)))->(!((B)|!(B))->((B)->((B)|!(B)))))->((((B)->((B)|!(B)))->((!((B)|!(B))->((B)->((B)|!(B))))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))))->(((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))))",
    "(((B)->((B)|!(B)))->((!((B)|!(B))->((B)->((B)|!(B))))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))))->(((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))))",
    "((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))",
    "(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))",
    "((((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))))->(((B)->((B)|!(B)))->((((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))))",
    "((B)->((B)|!(B)))->((((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))))",
    "((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))",
    "(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(((B)->((B)|!(B)))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))",
    "((B)->((B)|!(B)))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))",
    "(((B)->((B)|!(B)))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->((((B)->((B)|!(B)))->((((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))))->(((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))))",
    "(((B)->((B)|!(B)))->((((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))))->(((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))))",
    "((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))",
    "(((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B)))))->((((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))))->(((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))))",
    "(((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->((B)|!(B)))->(((B)->!((B)|!(B)))->!(B))))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))))->(((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))",
    "((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))",
    "(((B)->((B)|!(B)))->(!((B)|!(B))->(((B)->!((B)|!(B)))->!(B))))->((((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B))))->(((B)->((B)|!(B)))->(!((B)|!(B))->!(B))))",
    "(((B)->((B)|!(B)))->((!((B)|!(B))->(((B)->!((B)|!(B)))->!(B)))->(!((B)|!(B))->!(B))))->(((B)->((B)|!(B)))->(!((B)|!(B))->!(B)))",
    "((B)->((B)|!(B)))->(!((B)|!(B))->!(B))",
    "!((B)|!(B))->!(B)",
    "!(B)->((B)|!(B))",
    "(!((B)|!(B))->(!(B)->!((B)|!(B))))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B)))",
    "((!((B)|!(B))->(!(B)->!((B)|!(B))))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B))))->((!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->!((B)|!(B))))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B)))))",
    "(!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->!((B)|!(B))))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B))))",
    "!((B)|!(B))->(!(B)->!((B)|!(B)))",
    "(!((B)|!(B))->(!(B)->!((B)|!(B))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->(!(B)->!((B)|!(B)))))",
    "(!(B)->((B)|!(B)))->(!((B)|!(B))->(!(B)->!((B)|!(B))))",
    "((!(B)->((B)|!(B)))->(!((B)|!(B))->(!(B)->!((B)|!(B)))))->(((!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->!((B)|!(B))))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B)))))->((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B)))))",
    "((!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->!((B)|!(B))))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B)))))->((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B))))",
    "(!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B)))",
    "(!((B)|!(B))->(!(B)->((B)|!(B))))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))",
    "((!((B)|!(B))->(!(B)->((B)|!(B))))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))))->((!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->((B)|!(B))))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))))",
    "(!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->((B)|!(B))))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))))",
    "(!(B)->((B)|!(B)))->(!((B)|!(B))->(!(B)->((B)|!(B))))",
    "((!(B)->((B)|!(B)))->(!((B)|!(B))->(!(B)->((B)|!(B)))))->(((!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->((B)|!(B))))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))))->((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))))",
    "((!(B)->((B)|!(B)))->((!((B)|!(B))->(!(B)->((B)|!(B))))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))))->((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))))",
    "(!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))",
    "((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))",
    "(((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))))->((!(B)->((B)|!(B)))->(((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))))",
    "(!(B)->((B)|!(B)))->(((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))))",
    "(!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))",
    "((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->((!(B)->((B)|!(B)))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))",
    "(!(B)->((B)|!(B)))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))",
    "((!(B)->((B)|!(B)))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(((!(B)->((B)|!(B)))->(((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))))",
    "((!(B)->((B)|!(B)))->(((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))))",
    "(!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))",
    "((!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B)))))->(((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))))",
    "((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->((B)|!(B)))->((!(B)->!((B)|!(B)))->!!(B))))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))",
    "(!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))",
    "((!(B)->((B)|!(B)))->(!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B))))->(((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->!!(B))))",
    "((!(B)->((B)|!(B)))->((!((B)|!(B))->((!(B)->!((B)|!(B)))->!!(B)))->(!((B)|!(B))->!!(B))))->((!(B)->((B)|!(B)))->(!((B)|!(B))->!!(B)))",
    "(!(B)->((B)|!(B)))->(!((B)|!(B))->!!(B))",
    "(!((B)|!(B))->!!(B))",
    "(!((B)|!(B))->!(B))->((!((B)|!(B))->!(!(B)))->!(!((B)|!(B))))",
    "(!((B)|!(B))->!(!(B)))->!(!((B)|!(B)))",
    "!(!((B)|!(B)))",
    "!(!((B)|!(B)))->((B)|!(B))",
    "(B)|!(B)",
    "(B)->(A)",
    "!(B)->(A)",
    "((B)->(A))->((!(B)->(A))->(((B)|!(B))->(A)))",
    "(!(B)->(A))->(((B)|!(B))->(A))",
    "((B)|!(B))->(A)",
    "(A)"]
    
