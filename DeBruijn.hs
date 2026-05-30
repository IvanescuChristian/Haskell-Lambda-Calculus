module DeBruijn where

import Lambda (Lambda(..))

type Context = [String]

data DeBruijn = DBVar Int
              | DBFree String
              | DBApp DeBruijn DeBruijn
              | DBAbs String DeBruijn

instance Show DeBruijn where
    show (DBVar n) = show n
    show (DBFree x) = x
    show (DBApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (DBAbs _ e) = "λ " ++ show e

instance Eq DeBruijn where
    (DBVar n) == (DBVar m) = n == m
    (DBFree _) == (DBFree _) = True
    (DBApp e1 e2) == (DBApp f1 f2) = e1 == f1 && e2 == f2
    (DBAbs _ e) == (DBAbs _ f) = e == f
    _ == _ = False

-- 4.1.
findIdx :: String -> Context -> Maybe Int
findIdx _ [] = Nothing
findIdx x (y:ys) = case x == y of
    True  -> Just 0
    False -> case findIdx x ys of
        Nothing -> Nothing
        Just i  -> Just (i + 1)

toDB :: Context -> Lambda -> DeBruijn
toDB ctx expr = case expr of
    Var x     -> case findIdx x ctx of
        Nothing -> DBFree x
        Just i  -> DBVar i
    App e1 e2 -> DBApp (toDB ctx e1) (toDB ctx e2)
    Abs x e   -> DBAbs x (toDB (x:ctx) e)
    Macro m   -> DBFree m

-- 4.2.
fromDB :: Context -> DeBruijn -> Lambda
fromDB ctx expr = case expr of
    DBFree x      -> Var x
    DBVar n       -> Var (ctx !! n)
    DBApp e1 e2   -> App (fromDB ctx e1) (fromDB ctx e2)
    DBAbs name e  -> Abs name (fromDB (name:ctx) e)

-- 4.3.
isNormalForm :: DeBruijn -> Bool
isNormalForm expr = case expr of
    DBVar _              -> True
    DBFree _             -> True
    DBAbs _ e            -> isNormalForm e
    DBApp (DBAbs _ _) _  -> False
    DBApp e1 e2          -> isNormalForm e1 && isNormalForm e2

-- 4.4.
shiftFrom :: Int -> Int -> DeBruijn -> DeBruijn
shiftFrom cutoff amount expr = case expr of
    DBFree x      -> DBFree x
    DBVar n       -> case n >= cutoff of
        True  -> DBVar (n + amount)
        False -> DBVar n
    DBApp e1 e2   -> DBApp (shiftFrom cutoff amount e1) (shiftFrom cutoff amount e2)
    DBAbs name e  -> DBAbs name (shiftFrom (cutoff + 1) amount e)

reduceAt :: Int -> DeBruijn -> DeBruijn -> DeBruijn
reduceAt depth val expr = case expr of
    DBFree x      -> DBFree x
    DBVar n       -> case compare n depth of
        LT -> DBVar n
        EQ -> shiftFrom 0 depth val
        GT -> DBVar (n - 1)
    DBApp e1 e2   -> DBApp (reduceAt depth val e1) (reduceAt depth val e2)
    DBAbs name e  -> DBAbs name (reduceAt (depth + 1) val e)

reduce :: DeBruijn -> DeBruijn -> DeBruijn
reduce val expr = reduceAt 0 val expr

-- 4.5.
normalStep :: DeBruijn -> DeBruijn
normalStep expr = case expr of
    DBApp (DBAbs _ body) arg -> reduce arg body
    DBApp e1 e2 -> case isNormalForm e1 of
        False -> DBApp (normalStep e1) e2
        True  -> DBApp e1 (normalStep e2)
    DBAbs name e -> DBAbs name (normalStep e)
    _ -> expr

-- 4.6.
applicativeStep :: DeBruijn -> DeBruijn
applicativeStep expr = case expr of
    DBApp e1 e2 -> case isNormalForm e1 of
        False -> DBApp (applicativeStep e1) e2
        True  -> case isNormalForm e2 of
            False -> DBApp e1 (applicativeStep e2)
            True  -> case e1 of
                DBAbs _ body -> reduce e2 body
                _            -> expr
    DBAbs name e -> DBAbs name (applicativeStep e)
    _ -> expr

-- 4.7.
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify stepFn expr = case isNormalForm expr of
    True  -> [expr]
    False -> expr : simplify stepFn (stepFn expr)

normal :: DeBruijn -> [DeBruijn]
normal = simplify normalStep

applicative :: DeBruijn -> [DeBruijn]
applicative = simplify applicativeStep
