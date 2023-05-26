module Language.Diodorus.Eval where

import Language.Diodorus.Model
import qualified Language.Diodorus.Env as Env


evalExpr :: DEnv -> DEnv -> Expr -> Value
evalExpr globals env (Apply e es) =
    let
        actuals = map (evalExpr globals env) es
    in
        case evalExpr globals env e of
            Fun formals body lexicalEnv ->
                evalExpr globals (Env.extend lexicalEnv formals actuals) body
            Builtin Add ->
                (\[(Int x), (Int y)] -> Int (x + y)) actuals
            Builtin Mul ->
                (\[(Int x), (Int y)] -> Int (x * y)) actuals

evalExpr globals env (Name n) = case Env.fetch n env of
    Just v -> v
    Nothing -> case Env.fetch n globals of
        Just v -> v
        Nothing -> error $ "undefined name " ++ n

evalExpr globals env (Eval e) = case evalExpr globals env e of
    Quote qe -> evalExpr globals env qe
    _        -> error "type mismatch"

-- When we evaluate a literal function, we install in it the current environment.
evalExpr globals env (Lit (Fun formals body _)) = Fun formals body env

evalExpr globals env (Lit v) = v

--

makeInitialEnv (Program defns) = m defns where
    m [] = builtins
    m ((name, (Lit value)): rest) = Env.insert name value $ m rest
    m ((name, other): _) = error "non-literal toplevel"
    builtins = Env.extend Env.empty ["mul", "add"] [Builtin Mul, Builtin Add]
