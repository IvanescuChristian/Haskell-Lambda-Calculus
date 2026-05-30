module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.


addIfNew :: String -> [String] -> [String]
addIfNew xs x = case elem xs x of
    True -> x
    False -> xs : x
addAll ::[String] -> [String] -> [String]
addAll x acc = case x of
    [] -> acc
    (xs:x) -> addAll x (addIfNew xs acc)

vars :: Lambda -> [String]
vars expr = case expr of
    Var x -> [x]
    App e1 e2 -> addAll (vars e1) (vars e2)
    Abs x e -> addIfNew x (vars e)
    Macro _ -> []
--vars expr = nub(case expr of
--    Var x -> [x]
--    App e1 e2 -> vars e1 ++ vars e2
--    Abs x e -> [x] ++ vars e
--    Macro _ -> []) -- nub removes reoccuring elements as 2 not have dups.

-- 1.2.x
removeFromList :: String -> [String] -> [String]
removeFromList element lista = case lista of
    [] -> []
    (xs:x) -> case xs == element of
        True -> removeFromList element x
        False -> xs : removeFromList element x

freeVars :: Lambda -> [String]
freeVars expr = case expr of
    Var x -> [x]
    App e1 e2 -> addAll (freeVars e1) (freeVars e2)
    Abs x e -> removeFromList x (freeVars e)
    Macro _ -> []

-- 1.3.
stringsOfLen :: Int -> [String]
stringsOfLen 1 = [[c] | c <- ['a'..'z']]
stringsOfLen n = [c:s | c <- ['a'..'z'], s <- stringsOfLen (n-1)]

allCandidates :: Int -> [String]
allCandidates n = stringsOfLen n ++ allCandidates (n + 1)

findFirst :: [String] -> [String] -> String
findFirst taken (x:xs) = case elem x taken of
    True  -> findFirst taken xs
    False -> x

newVar :: [String] -> String
newVar taken = findFirst taken (allCandidates 1)

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm = undefined

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce = undefined

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep = undefined

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep = undefined

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify = undefined

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
