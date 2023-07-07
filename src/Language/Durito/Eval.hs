module Language.Durito.Eval where

import Language.Durito.Model
import qualified Language.Durito.Env as Env


evalExpr :: VEnv -> VEnv -> Expr -> Value
evalExpr globals env (Apply e es) =
    let
        actuals = map (evalExpr globals env) es
        builtinEval [(Quote qe)] = evalExpr globals Env.empty qe
        builtinEval other = error ("type mismatch: " ++ show other)
    in
        case evalExpr globals env e of
            Fun formals body lexicalEnv ->
                evalExpr globals (Env.extend lexicalEnv formals actuals) body
            Builtin Add ->
                (\[(Int x), (Int y)] -> Int (x + y)) actuals
            Builtin Mul ->
                (\[(Int x), (Int y)] -> Int (x * y)) actuals
            Builtin Eval ->
                builtinEval actuals

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

-- When we evaluate a literal function, we install in it the current environment.
-- NOTE, this relies on the fact that literal functions have empty lexical envs.
-- TODO: maybe we should assert that.  (Or fiddle with the data types to obviate it...)
evalExpr globals env (Lit (Fun formals body _)) = Fun formals body env

evalExpr globals env (Lit v) = v

--

evalProgram :: Program -> [Value] -> Value
evalProgram program actuals =
    let
        globals = makeInitialEnv program
    in case Env.fetch "main" globals of
        Nothing ->
            error "No main function defined"
        Just (Fun formals main _) ->
            evalExpr globals (Env.extend Env.empty formals actuals) main

--

makeInitialEnv (Program defns) = m defns where
    m [] = builtins
    m ((name, value): rest) = Env.insert name value $ m rest
    builtins = Env.extend Env.empty ["mul", "add", "eval"] [Builtin Mul, Builtin Add, Builtin Eval]
