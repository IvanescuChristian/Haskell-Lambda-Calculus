module DeBruijn where

import Lambda (Lambda(..))

type Context = [String]

-- la reduce si apelurile lui , e1 e2 se pun e2 e1. 
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
findIdx :: Context -> String -> Maybe Int
findIdx [] _ = Nothing
findIdx (xs:ctx) x = case xs == x of
    True -> Just 0
    False -> case findIdx ctx x of
        Nothing -> Nothing
        Just i -> Just (i+1)
toDB :: Context -> Lambda -> DeBruijn
toDB ctx e = case e of
    Macro m -> DBFree m
    Var x -> case (findIdx ctx x) of
        Nothing -> DBFree x
        Just i -> DBVar i
    Abs x e -> DBAbs x (toDB (x:ctx) e)
    App e1 e2 -> DBApp (toDB ctx e1) (toDB ctx e2)
-- 4.2.
findElementIdx :: Context -> Int -> String
findElementIdx [] _ = "Error"
findElementIdx (xs:ctx) idx = case idx == 0 of
    True -> xs
    False -> findElementIdx ctx (idx-1)

fromDB :: Context -> DeBruijn -> Lambda
fromDB ctx db = case db of
    DBFree x -> Var x
    --DBVar x -> Var (ctx !! x)
    DBVar idx -> case (findElementIdx ctx idx) of
        "Error" -> Var "Error"
        s -> Var s
    DBAbs x e -> Abs x (fromDB (x:ctx) e)
    DBApp e1 e2-> App (fromDB ctx e1) (fromDB ctx e2)
-- 4.3.
isNormalForm :: DeBruijn -> Bool
isNormalForm db = case db of
    DBFree m -> True
    DBVar x -> True
    DBAbs x e -> (isNormalForm e)
    DBApp (DBAbs _ _) _ -> False 
    DBApp e1 e2 -> (isNormalForm e1) && (isNormalForm e2)
-- 4.4.
shiftFrom :: Int -> Int -> DeBruijn -> DeBruijn
shiftFrom st fi db = case db of
    DBFree m -> DBFree m
    DBVar x -> case x >= st of
        True -> DBVar (x+st)
        False -> DBVar x
    DBAbs x e -> DBAbs x (shiftFrom (st+1) fi e)
    DBApp e1 e2 -> DBApp (shiftFrom st fi e1) (shiftFrom st fi e2)
reduceAt :: Int -> DeBruijn -> DeBruijn -> DeBruijn
reduceAt idx db1 db2 = case db1 of
    DBFree m -> DBFree m
    DBVar x -> case compare x idx of
        LT -> DBVar x
        EQ -> shiftFrom 0 idx db2
        GT -> DBVar (x-1)
    DBAbs x e1 -> DBAbs x (reduceAt (idx+1) e1 db2)
    DBApp e1 e2 -> DBApp (reduceAt idx e1 db2) (reduceAt idx e2 db2)
reduce :: DeBruijn -> DeBruijn -> DeBruijn
reduce db1 db2 = (reduceAt 0 db2 db1) 
--testul le cheama val expr deci trb sa le dau invers 
-- 4.5.
normalStep :: DeBruijn -> DeBruijn
normalStep db = case db of
    DBAbs x e -> DBAbs x (normalStep e)
    DBApp (DBAbs _ e1) e2 -> (reduce e2 e1) --orice apel de reduce are argumentele inversate
    DBApp e1 e2 -> case (isNormalForm e1) of
        True -> DBApp e1 (normalStep e2)
        False -> DBApp (normalStep e1) e2
    _ -> db

-- 4.6.
applicativeStep :: DeBruijn -> DeBruijn
applicativeStep db = case db of
    DBAbs x e -> DBAbs x (applicativeStep e)
    DBApp e1 e2 -> case (isNormalForm e1) of
        True -> case (isNormalForm e2) of
            True ->case e1 of
                DBAbs y f1 -> reduce e2 f1
                _ -> db
            False ->DBApp e1 (applicativeStep e2)
        False -> DBApp (applicativeStep e1) e2
    _ -> db

-- 4.7.
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify stepFn db = case isNormalForm db of
    True -> [db]
    False -> db : simplify stepFn (stepFn db)

normal :: DeBruijn -> [DeBruijn]
normal = simplify normalStep

applicative :: DeBruijn -> [DeBruijn]
applicative = simplify applicativeStep
