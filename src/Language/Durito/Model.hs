module Language.Durito.Model where

import qualified Language.Durito.Env as Env

--
-- A "VEnv" maps names to values and is used
-- to interpret the program at runtime.
--

type Name = String

data Value = Fun [Name] Expr VEnv
           | Quote Expr
           | Int Integer
           | Cons Value Value     -- shall not appear in Lits - use `cons` builtin
           | Nil
           | Builtin Builtin
    deriving (Show, Ord, Eq)

type VEnv = Env.Env Name Value

--
-- AST of the program.
--
-- Note that literal values in the program source,
-- including literal function values, are simply
-- a Lit node containing a Value (defined above).
-- Some Values may not appear in a Lit (or have
-- restrictions on them when they are.)
--

data Program = Program [(Name, Value)]
    deriving (Show, Ord, Eq)

data Expr = Apply Expr [Expr]
          | Name Name
          | Lit Value
          | Subst [(Name, Expr)] Expr
    deriving (Show, Ord, Eq)

freeVars ::  [Name] -> Expr -> [Name]
freeVars b (Apply app exprs) =
    (freeVars b app) ++ (freeVarsAll b exprs)
freeVars b (Name n) =
    if n `elem` b then [] else [n]
freeVars b (Lit (Fun formals body _)) =
    freeVars (b ++ formals) body
freeVars b (Lit _) =
    []
freeVars b (Subst bindings body) =
    (freeVars b body) ++ (freeVarsAll b (map (snd) bindings))

freeVarsAll b exprs =
    foldr (\expr acc -> acc ++ (freeVars b expr)) [] exprs

mapProgram f (Program defns) = Program (map f defns)

mapBindings f = map (\(name, expr) -> (name, f expr))

--
-- Builtins (putting these in their own module would be circular)
--

data Builtin = DuritoAdd
             | DuritoMul
             | DuritoEval
             | DuritoCons
             | DuritoSubst
    deriving (Show, Ord, Eq)

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
substBinding name value (Subst bindings body) =
    let
        bindings' = mapBindings (substBinding name value) bindings
    in
        Subst bindings' (substBinding name value body)
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
        convertBindings (Cons (Cons k v) tail) =
            ((k, v):convertBindings tail)
    in
        -- FIXME!
        Quote (substBindings [] expr)
