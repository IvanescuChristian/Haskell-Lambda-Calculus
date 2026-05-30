module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isLower, isUpper, isDigit)

import Lambda
import Code

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser (\input -> case parse p input of
        Nothing        -> Nothing
        Just (x, rest) -> Just (f x, rest))

instance Applicative Parser where
    pure x    = Parser (\input -> Just (x, input))
    pf <*> px = Parser (\input -> case parse pf input of
        Nothing        -> Nothing
        Just (f, rest) -> case parse px rest of
            Nothing         -> Nothing
            Just (x, rest2) -> Just (f x, rest2))

instance Monad Parser where
    return = pure
    p >>= f = Parser (\input -> case parse p input of
        Nothing        -> Nothing
        Just (x, rest) -> parse (f x) rest)

instance Alternative Parser where
    empty   = Parser (\_ -> Nothing)
    p <|> q = Parser (\input -> case parse p input of
        Nothing  -> parse q input
        Just res -> Just res)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\input -> case input of
    []     -> Nothing
    (c:cs) -> case predicate c of
        True  -> Just (c, cs)
        False -> Nothing)

charP :: Char -> Parser Char
charP c = satisfy (== c)

parseVarName :: Parser String
parseVarName = do
    c  <- satisfy isLower
    cs <- many (satisfy isLower)
    return (c:cs)

parseMacroName :: Parser String
parseMacroName = do
    c  <- satisfy (\x -> isUpper x || isDigit x)
    cs <- many (satisfy (\x -> isUpper x || isDigit x))
    return (c:cs)

parseLambdaExpr :: Parser Lambda
parseLambdaExpr = parseAbs <|> parseApp

parseAbs :: Parser Lambda
parseAbs = do
    charP '\\'
    x    <- parseVarName
    charP '.'
    body <- parseLambdaExpr
    return (Abs x body)

parseApp :: Parser Lambda
parseApp = do
    first <- parseAtom
    rest  <- many spaceAtom
    return (foldl App first rest)

spaceAtom :: Parser Lambda
spaceAtom = do
    charP ' '
    parseAtom

parseAtom :: Parser Lambda
parseAtom = parseParen <|> parseMacroExpr <|> parseVarExpr

parseParen :: Parser Lambda
parseParen = do
    charP '('
    e <- parseLambdaExpr
    charP ')'
    return e

parseMacroExpr :: Parser Lambda
parseMacroExpr = do
    name <- parseMacroName
    return (Macro name)

parseVarExpr :: Parser Lambda
parseVarExpr = do
    x <- parseVarName
    return (Var x)

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda input = case parse parseLambdaExpr input of
    Just (expr, _) -> expr
    Nothing        -> error ("Parse error: " ++ input)

-- 3.3.
parseLine :: String -> Either String Line
parseLine input = case parse parseLineParser input of
    Nothing        -> Left ("Parse error: " ++ input)
    Just (line, _) -> Right line

parseLineParser :: Parser Line
parseLineParser = parseBindingLine <|> parseEvalLine

parseBindingLine :: Parser Line
parseBindingLine = do
    name <- parseMacroName
    charP '='
    e    <- parseLambdaExpr
    return (Binding name e)

parseEvalLine :: Parser Line
parseEvalLine = do
    e <- parseLambdaExpr
    return (Eval e)
