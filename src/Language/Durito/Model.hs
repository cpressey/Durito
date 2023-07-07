module Language.Durito.Model where

import qualified Language.Durito.Env as Env

--
-- A "VEnv" maps names to values and is used
-- to interpret the program at runtime.
--
-- A "KEnv", meanwhile, maps names to the knowledge
-- about values ahead-of-time (static analysis).
--
-- There are a number of possibilities for making a
-- data structure that can represent either of these.
-- They are all contrived.  The pollution of having
-- both here, was chosen because it seems one of the
-- less contrived solutions.
--

type Name = String

data Value = Fun [Name] Expr VEnv KEnv
           | Quote Expr
           | Int Integer
           | Builtin Builtin
    deriving (Show, Ord, Eq)

type VEnv = Env.Env Name Value

data KnownStatus = Known Value
    deriving (Show, Ord, Eq)

type KEnv = Env.Env Name KnownStatus

--
-- AST of the program.
--
-- Note that literal values in the program source,
-- including literal function values, are simply
-- a Lit node containing a Value (defined above).
--

data Program = Program [(Name, Value)]
    deriving (Show, Ord, Eq)

data Expr = Apply Expr [Expr]
          | Name Name
          | Eval Expr
          | Lit Value
          | Subst [(Name, Expr)] Expr
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
