module Language.Durito.Builtins where

import Language.Durito.BuiltinType
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

evalBuiltin _ DuritoAdd [(Int x), (Int y)] =
    Int (x + y)
evalBuiltin _ DuritoMul [(Int x), (Int y)] =
    Int (x * y)
evalBuiltin _ DuritoCons [x, y] =
    Cons x y
evalBuiltin evaluator DuritoEval [(Quote qe venv)] =
    -- TODO: injecy venv into evaluator when evaluating
    evaluator qe
evalBuiltin _ DuritoSubst [bindings, (Quote expr venv)] =
    -- NOTE: venv is not handled well here
    let
        pairs = convertBindings bindings
        convertBindings Nil = []
        convertBindings (Cons (Cons (Quote (Name n) _) (Cons (Quote (Lit v) _) Nil)) tail) =
            ((n, v):convertBindings tail)
        convertBindings (Cons (Cons (Quote (Name n) _) (Cons v Nil)) tail) =
            ((n, v):convertBindings tail)
    in
        Quote (substBindings pairs expr) venv
evalBuiltin _ other args =
    error $ "type mismatch: " ++ (show other) ++ " " ++ (show args)

builtinsEnv = Env.extend Env.empty
    ["mul", "add", "eval", "cons", "nil", "subst"]
    [Builtin DuritoMul, Builtin DuritoAdd, Builtin DuritoEval, Builtin DuritoCons, Nil, Builtin DuritoSubst]
