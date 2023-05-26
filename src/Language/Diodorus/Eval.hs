module Language.Diodorus.Eval where

import Language.Diodorus.Model
import Language.Diodorus.Env


evalExpr :: Env -> Expr -> Expr

evalExpr env other = other   -- literals

-- evalExpr env (Apply e args) =
--     let
--         f = evalExpr p e
--         args' = map (evalExpr p) args
--     in
--         evalFun p f args'

