module Language.Diodorus.Eval where

import Language.Diodorus.Model
import Language.Diodorus.Env


evalExpr :: DEnv -> Expr -> Value
evalExpr env (Apply e es) =
    let
        actuals = map (evalExpr env) es
    in
        case evalExpr env e of
            Fun formals body ->
                evalFun env formals actuals body
            Builtin Add ->
                (\[(Int x), (Int y)] -> Int (x + y)) actuals
            Builtin Mul ->
                (\[(Int x), (Int y)] -> Int (x * y)) actuals

evalExpr env (Name n) = case fetch n env of
    Just v -> v
    Nothing -> error $ "undefined name " ++ n
evalExpr env (Eval e) = evalExpr env e
evalExpr env (Lit v) = v


evalFun :: DEnv -> [Name] -> [Value] -> Expr -> Value
evalFun env formals actuals body =
    -- FIXME: this is dynamic scoping -- make it lexical
    evalExpr (extend env formals actuals) body

makeInitialEnv [] = builtins
makeInitialEnv ((name, (Lit value)): rest) = insert name value $ makeInitialEnv rest
makmakeInitialEnveEnv ((name, other): _) = error "non-literal toplevel"

builtins = extend empty ["mul", "add"] [Builtin Mul, Builtin Add]
