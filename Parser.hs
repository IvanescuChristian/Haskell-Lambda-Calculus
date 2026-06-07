module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Code

import Data.Char

-- functor : fmap ; applicative : pure , pf <*> px; monad return = pure , p >>= f ; alternative empty , <|> .
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser(
        \input -> case parse p input of
            Nothing -> Nothing
            Just(xs, x) -> Just(f xs, x)
        )
instance Applicative Parser where
    pure x = Parser(\input -> Just(x,input))
    pf <*> px = Parser(
        \input -> case parse pf input of
            Nothing -> Nothing
            Just(f, rest) -> case parse px rest of
                Nothing -> Nothing
                Just(xs, x) -> Just(f xs, x)
        )
instance Monad Parser where
    return = pure
    p >>= f =Parser(
        \input -> case parse p input of
            Nothing -> Nothing
            Just(xs, x) -> parse (f xs) x
        )
instance Alternative Parser where
    empty = Parser(\_ -> Nothing)
    p <|> q = Parser(
        \input -> case parse p input of
            Nothing -> parse q input
            Just ceva -> Just ceva
        )
-- 2.1. / 3.2.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser(
    \input -> case input of
        [] -> Nothing
        (cs:c) -> case f cs of
            True -> Just(cs,c)
            False-> Nothing
    )
charP :: Char -> Parser Char
charP c = satisfy ( ==c)

parseParen :: Parser Lambda
parseParen = do
    charP '('
    e <- parseLambdaExpr
    charP ')'
    return e

parseAtom :: Parser Lambda
parseAtom = parseParen <|> fmap Macro parseMacroName <|> fmap Var parseVarName

spaceAtom :: Parser Lambda
spaceAtom = do
    charP ' '
    parseAtom

parseMacroName :: Parser String
parseMacroName = do
    cs <- satisfy (\x -> isUpper x || isDigit x)
    c <- many (satisfy (\x -> isUpper x || isDigit x) )
    return (cs:c)
parseVarName :: Parser String
parseVarName = do
    cs <- satisfy (\x -> isLower x)
    c <- many (satisfy (\x -> isLower x))
    return (cs:c)
parseApp :: Parser Lambda
parseApp = do
    as <- parseAtom
    a <- many spaceAtom
    return (foldl App as a)

parseAbs :: Parser Lambda
parseAbs = do    
    charP '\\'
    x <- parseVarName
    charP '.'
    e <- parseLambdaExpr
    return (Abs x e)

parseLambdaExpr :: Parser Lambda
parseLambdaExpr = parseApp <|> parseAbs

parseLambda :: String -> Lambda
parseLambda input = case (parse parseLambdaExpr input) of
    Nothing -> error("Eroare")
    Just (expr, _) -> expr
-- 3.3.
parseLine :: String -> Either String Line
parseLine input = case parse parseLineExpr input of
    Nothing -> Left "Nothing"
    Just(line, _) -> Right line

parseLineExpr :: Parser Line
parseLineExpr = parseBindingLine <|> parseEvalLine

parseBindingLine :: Parser Line
parseBindingLine = do
    name <- parseMacroName
    charP '='
    e <- parseLambdaExpr
    return (Binding name e)
parseEvalLine :: Parser Line
parseEvalLine = do
    e <- parseLambdaExpr
    return (Eval e)
