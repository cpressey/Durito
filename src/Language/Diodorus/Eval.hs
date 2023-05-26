module Language.Diodorus.Eval where

import Language.Diodorus.Model
import Language.Diodorus.Env


evalExpr :: Env -> Expr -> Value
evalExpr env (Apply e es) =
    let
        Fun formals body = evalExpr env e
        actuals = map (evalExpr env) es
    in
        evalFun env formals actuals body
evalExpr env (Name n) = case fetch n env of
    Just v -> v
    Nothing -> error $ "undefined name " ++ n
evalExpr env (Eval e) = evalExpr env e
evalExpr env (Lit v) = v


evalFun :: Env -> [Name] -> [Value] -> Expr -> Value
evalFun env formals actuals body =
    evalExpr (extendEnv env formals actuals) body

extendEnv :: Env -> [Name] -> [Value] -> Env
extendEnv env [] [] = env
extendEnv env (formal:formals) (actual:actuals) =
    extendEnv (insert formal actual env) formals actuals
