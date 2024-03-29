{-# LANGUAGE FlexibleContexts #-}
module Language.Durito.Parser where

import Text.ParserCombinators.Parsec

import qualified Language.Durito.Env as Env
import Language.Durito.Model


program = do
    fspaces
    ds <- many defn
    return $ Program ds

defn = do
    keyword "def"
    n <- name
    keyword "="
    x <- literal
    return (n, x)

literal = litFun <|> litQuote <|> litInt

litFun = do
    keyword "fun"
    keyword "("
    f <- sepBy (name) (keyword ",")
    keyword ")"
    keyword "->"
    e <- expr
    return $ Fun f e Env.empty

litQuote = do
    keyword "<<"
    e <- expr
    keyword ">>"
    fspaces
    return $ Quote e

litInt = do
    c <- digit
    cs <- many digit
    num <- return (read (c:cs) :: Integer)
    fspaces
    return $ Int num

expr = (try exprList) <|> (try exprLet) <|> (try exprLiteral) <|> (try exprApply) <|> exprName

exprList = do
    keyword "["
    es <- sepBy (expr) (keyword ",")
    keyword "]"
    return $ listToCons es
    where
        listToCons [] = (Name "nil")
        listToCons (e:rest) = Apply (Name "cons") [e, listToCons rest]

exprLet = do
    keyword "let"
    bindings <- sepBy (binding) (keyword ",")
    keyword "in"
    body <- expr
    return $ Let bindings body

binding = do
    n <- name
    keyword "="
    e <- expr
    return (n, e)

exprLiteral = do
    v <- literal
    return $ Lit v

exprApply = do
    e <- (try subExpr) <|> exprName
    keyword "("
    a <- sepBy (expr) (keyword ",")
    keyword ")"
    return $ Apply e a

subExpr = do
    keyword "("
    e <- expr
    keyword ")"
    return e

exprName = do
    n <- name
    return $ Name n

--

keyword s = do
    try (string s)
    fspaces

fspaces = do
    spaces
    return ()

name = do
    c <- lower
    s <- many (alphaNum)
    fspaces
    return (c:s)

--

parseDurito :: String -> Either ParseError Program
parseDurito text = parse program "" text

parseLiteral text = case parse literal "" text of
    Right v -> v
    other -> error ("cannot parse literal: " ++ show other)
