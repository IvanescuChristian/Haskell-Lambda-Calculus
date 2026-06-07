module Code where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
expandMacros :: Context -> Lambda -> Either String Lambda
expandMacros ctx expr = case expr of
    Var x     -> Right (Var x)
    Macro m   -> case lookup m ctx of
        Nothing -> Left ("Undefined macro: " ++ m)
        Just e  -> Right e
    Abs x e   -> case expandMacros ctx e of
        Left err -> Left err
        Right ex -> Right (Abs x ex)
    App e1 e2 -> case expandMacros ctx e1 of
        Left err  -> Left err
        Right ex1 -> case expandMacros ctx e2 of
            Left err  -> Left err
            Right ex2 -> Right (App ex1 ex2)

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx stepFn expr = case expandMacros ctx expr of
    Left err       -> Left err
    Right expanded -> Right (simplify stepFn expanded)

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
