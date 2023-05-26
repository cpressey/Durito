{-# LANGUAGE FlexibleContexts #-}
module Language.Diodorus.Parser where

import Text.ParserCombinators.Parsec

import Language.Diodorus.Model


program = many defn

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
    return $ LitFun f e

litQuote = do
    keyword "<<"
    e <- expr
    keyword ">>"
    fspaces
    return $ LitQuote e

litInt = do
    c <- digit
    cs <- many digit
    num <- return (read (c:cs) :: Integer)
    fspaces
    return $ LitInt num

expr = (try exprApply) <|> (try exprName) <|> exprEval <|> literal

exprApply = do
    n <- name  -- Note, should really be an expression; but pretend there is a let
    keyword "("
    a <- sepBy (expr) (keyword ",")
    keyword ")"
    let e = Name n
    return $ Apply e a

exprName = do
    n <- name
    return $ Name n

exprEval = do
    keyword "eval"
    e <- expr
    return $ Eval e

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

parseDiodorus text = parse program "" text
