module Language.Durito.Eval where

import Language.Durito.Model
import qualified Language.Durito.Env as Env


evalExpr :: VEnv -> VEnv -> Expr -> Value
evalExpr globals env (Apply e es) =
    let
        actuals = map (evalExpr globals env) es
    in
        case evalExpr globals env e of
            Fun formals body lexicalEnv ->
                evalExpr globals (Env.extend lexicalEnv formals actuals) body
            Builtin DuritoEval ->
                -- Special case, this is here to avoid circularity
                let
                    builtinEval [(Quote qe)] = evalExpr globals Env.empty qe
                    builtinEval other = error ("type mismatch: " ++ show other)
                in
                    builtinEval actuals
            Builtin bi ->
                (evalBuiltin bi) actuals

evalExpr globals env (Name n) = case Env.fetch n env of
    Just v -> v
    Nothing -> case Env.fetch n globals of
        Just v -> v
        Nothing -> error $ "undefined name " ++ n

evalExpr globals env (Subst bindings e) =
    let
        evaledBindings = mapBindings (evalExpr globals env) bindings
    in case evalExpr globals env e of
        Quote expr -> Quote (substBindings evaledBindings expr)
        _          -> error "type mismatch"

evalExpr globals env (Lit (Fun formals body lexicalEnv)) =
    if lexicalEnv == Env.empty then (Fun formals body env) else
        error "assertion failed: function literal already has a lexical env"

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
    m [] = builtins
    m ((name, value): rest) = Env.insert name value $ m rest
    builtins = Env.extend Env.empty
        ["mul", "add", "eval", "cons", "nil", "substx"]
        [Builtin DuritoMul, Builtin DuritoAdd, Builtin DuritoEval, Builtin DuritoCons, Nil, Builtin DuritoSubst]
