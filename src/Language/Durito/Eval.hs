module Language.Durito.Eval where

--import Debug.Trace

import Language.Durito.Model
import Language.Durito.Builtins
import qualified Language.Durito.Env as Env


evalExpr :: VEnv -> VEnv -> Expr -> Value
evalExpr globals env (Apply e es) =
    let
        actuals = map (evalExpr globals env) es
        evaluator venv expr =
            --traceShow (venv,expr)
                evalExpr globals venv expr
    in
        case evalExpr globals env e of
            Fun formals body lexicalEnv ->
                evalExpr globals (Env.extend lexicalEnv formals actuals) body
            Builtin bi ->
                (evalBuiltin evaluator bi) actuals

evalExpr globals env (Name n) = case Env.fetch n env of
    Just v -> v
    Nothing -> case Env.fetch n globals of
        Just v -> v
        Nothing -> error $ "undefined name " ++ n

evalExpr globals env (Lit (Fun formals body lexicalEnv)) =
    if lexicalEnv == Env.empty then (Fun formals body env) else
        error "assertion failed: function literal already has a lexical env"

evalExpr globals env (Lit (Quote expr lexicalEnv)) =
    if lexicalEnv == Env.empty then (Quote expr env) else
        error "assertion failed: quoted form literal already has a lexical env"

evalExpr globals env (Lit v) = v


evalProgram :: Program -> [Value] -> Value
evalProgram program actuals =
    let
        globals = makeInitialEnv program
    in case Env.fetch "main" globals of
        Nothing ->
            error "No main function defined"
        Just (Fun formals main _) ->
            evalExpr globals (Env.extend Env.empty formals actuals) main


makeInitialEnv :: Program -> VEnv
makeInitialEnv (Program defns) = m defns where
    m [] = builtinsEnv
    m ((name, value): rest) = Env.insert name value $ m rest
