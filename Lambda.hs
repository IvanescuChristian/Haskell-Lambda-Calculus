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
addAll :: [String] -> [String] -> [String]
addAll x acc = case x of 
  [] -> acc
  (xs : x) -> addAll x (addIfNew xs acc)
addIfNew :: String -> [String] -> [String]
addIfNew xs e = case e of
  [] -> [xs]
  e -> case elem xs e of
    True-> e
    False-> xs : e
vars :: Lambda -> [String]
vars expr = case expr of 
  Var x -> [x]
  Abs x e -> addIfNew x (vars e)
  App e1 e2 -> addAll (vars e1) (vars e2)
  Macro _ -> []

-- 1.2.
removeFromList :: String -> [String] -> [String]
removeFromList item e = case e of
  [] -> []
  (element: x) -> case element == item of
    True-> removeFromList item x
    False-> element : removeFromList item x
freeVars :: Lambda -> [String]
freeVars expr = case expr of 
  Var x -> [x]
  Abs x e -> removeFromList x (freeVars e)
  App e1 e2 -> addAll (freeVars e1) (freeVars e2)
  Macro _ -> []

-- 1.3.
generateStringsOfLenN :: Int -> [String]
generateStringsOfLenN n = case n of 
  1 -> [[c] | c <- ['a'..'z']]
  size -> [c : s | c <- ['a'..'z'], s <- (generateStringsOfLenN (size-1) )]

generateAllString :: Int -> [String]
generateAllString n = (generateStringsOfLenN n) ++ (generateAllString (n+1) )

findFirstDifferent :: [String] -> [String] -> String
findFirstDifferent e1 (element:e2) = case e1 of
  [] -> element
  lista -> case elem element e1 of
    True-> findFirstDifferent e1 e2
    False-> element
newVar :: [String] -> String
newVar taken = findFirstDifferent taken (generateAllString 1)

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm expr = case expr of
  Var x -> True
  Macro _ -> True
  Abs x e -> isNormalForm e
  App (Abs _ _) _ -> False
  App e1 e2 -> isNormalForm e1 && isNormalForm e2
-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = case e1 of
  Var y -> case y == x of
    True -> e2
    False-> Var y
  Macro m -> Macro m
  App f1 f2 -> App (reduce x f1 e2) (reduce x f2 e2)
  Abs y f1 -> case y == x of
    True -> Abs y f1
    False ->
      let
        new_var = newVar(freeVars f1 ++ freeVars e2 ++ [x])
        new_f1 = (reduce y f1 (Var new_var) )
      in Abs new_var (reduce x new_f1 e2)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep expr = case expr of
  Abs x e -> Abs x (normalStep e)
  App (Abs x e1) e2 -> reduce x e1 e2
  App e1 e2 -> case isNormalForm e1 of
    True -> App e1 (normalStep e2)
    False -> App (normalStep e1) e2
  _ -> expr

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep expr = case expr of
  Abs x e -> Abs x (applicativeStep e)
  App e1 e2 -> case isNormalForm e1 of
    True -> case isNormalForm e2 of
      True -> case e1 of
        Abs x e -> reduce x e e2
        _ -> expr
      False -> App e1 (applicativeStep e2)
    False -> App (applicativeStep e1) e2
  _ -> expr

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify stepFn e = case (isNormalForm e) of
  True -> [e]
  False -> e : simplify stepFn (stepFn e)
normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
