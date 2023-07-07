module Language.Durito.Builtins where

import qualified Language.Durito.Env as Env
import Language.Durito.Model


renderBuiltin DuritoAdd = "add"
renderBuiltin DuritoMul = "mul"
renderBuiltin DuritoEval = "eval"
renderBuiltin DuritoCons = "cons"
renderBuiltin DuritoSubst = "subst"

--
-- Implementation of "subst"
--

substBindings :: [(Name, Value)] -> Expr -> Expr
substBindings [] expr = expr
substBindings ((name, value):rest) expr =
    substBindings rest (substBinding name value expr)

substBinding :: Name -> Value -> Expr -> Expr
substBinding name value (Apply e1 es) =
    Apply (substBinding name value e1) (map (substBinding name value) es)
substBinding name value expr@(Name n) =
    if name == n then (Lit value) else expr
substBinding name value other =
    other

evalBuiltin DuritoAdd [(Int x), (Int y)] =
    Int (x + y)
evalBuiltin DuritoMul [(Int x), (Int y)] =
    Int (x * y)
evalBuiltin DuritoCons [x, y] =
    Cons x y
evalBuiltin DuritoSubst [bindings, (Quote expr)] =
    let
        pairs = convertBindings bindings
        convertBindings Nil = []
        convertBindings (Cons (Cons (Quote (Name n)) (Cons (Quote (Lit v)) Nil)) tail) =
            ((n, v):convertBindings tail)
        convertBindings (Cons (Cons (Quote (Name n)) (Cons v Nil)) tail) =
            ((n, v):convertBindings tail)
    in
        Quote (substBindings pairs expr)
evalBuiltin other args =
    error $ (show other) ++ (show args)

builtinsEnv = Env.extend Env.empty
    ["mul", "add", "eval", "cons", "nil", "subst"]
    [Builtin DuritoMul, Builtin DuritoAdd, Builtin DuritoEval, Builtin DuritoCons, Nil, Builtin DuritoSubst]
