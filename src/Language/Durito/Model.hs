module Language.Durito.Model where

import qualified Language.Durito.Env as Env

type Name = String

type DEnv = Env.Env Name Value


data Program = Program [(Name, Expr)]
    deriving (Show, Ord, Eq)

data Expr = Apply Expr [Expr]
          | Name Name
          | Eval Expr
          | Lit Value
          | Subst [(Name, Expr)] Expr
    deriving (Show, Ord, Eq)

data Value = Fun [Name] Expr DEnv
           | Quote Expr
           | Int Integer
           | Builtin Builtin
    deriving (Show, Ord, Eq)

data Builtin = Add | Mul
    deriving (Show, Ord, Eq)

--

mapProgram f (Program defns) = Program (map f defns)

--

mapBindings f = map (\(name, expr) -> (name, f expr))

substBindings :: [(Name, Value)] -> Expr -> Expr
substBindings [] expr = expr
substBindings ((name, value):rest) expr =
    substBindings rest (substBinding name value expr)

substBinding :: Name -> Value -> Expr -> Expr
substBinding name value (Apply e1 es) =
    Apply (substBinding name value e1) (map (substBinding name value) es)
substBinding name value expr@(Name n) =
    if name == n then (Lit value) else expr
substBinding name value (Eval expr) =
    Eval (substBinding name value expr)
substBinding name value (Subst bindings body) =
    let
        bindings' = mapBindings (substBinding name value) bindings
    in
        Subst bindings' (substBinding name value body)
substBinding name value other =
    other
