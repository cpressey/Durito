module Language.Diodorus.Eval where

import Language.Diodorus.Model
import Language.Diodorus.Env


evalExpr :: DEnv -> DEnv -> Expr -> Value
evalExpr globals env (Apply e es) =
    let
        actuals = map (evalExpr globals env) es
    in
        case evalExpr globals env e of
            Fun formals body lexicalEnv ->
                evalExpr globals (extend lexicalEnv formals actuals) body
            Builtin Add ->
                (\[(Int x), (Int y)] -> Int (x + y)) actuals
            Builtin Mul ->
                (\[(Int x), (Int y)] -> Int (x * y)) actuals

evalExpr globals env (Name n) = case fetch n env of
    Just v -> v
    Nothing -> case fetch n globals of
        Just gv -> gv
        Nothing -> error $ "undefined name " ++ n
evalExpr globals env (Eval e) = evalExpr globals env e
evalExpr globals env (Lit (Fun formals body _)) = Fun formals body env
evalExpr globals env (Lit v) = v

--

makeInitialEnv [] = builtins
makeInitialEnv ((name, (Lit value)): rest) = insert name value $ makeInitialEnv rest
makeInitialEnv ((name, other): _) = error "non-literal toplevel"

builtins = extend empty ["mul", "add"] [Builtin Mul, Builtin Add]
