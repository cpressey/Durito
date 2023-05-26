module Language.Diodorus.Eval where

import Language.Diodorus.Model
import Language.Diodorus.Env


evalExpr :: Env -> Expr -> Value

evalExpr env (Apply e args) = error "apply not yet implemented"
evalExpr env (Name n) = case fetch n env of
    Just v -> v
    Nothing -> error "undefined name"
evalExpr env (Eval e) = evalExpr env e
evalExpr env (Lit v) = v

-- evalExpr env (Apply e args) =
--     let
--         f = evalExpr p e
--         args' = map (evalExpr p) args
--     in
--         evalFun p f args'

