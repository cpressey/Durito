module Language.Durito.Model where

import qualified Language.Durito.Env as Env
import Language.Durito.BuiltinType

--
-- A "VEnv" maps names to values and is used
-- to interpret the program at runtime.
--

type Name = String

data Value = Fun [Name] Expr VEnv
           | Quote Expr VEnv
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

freeVarsAll b exprs =
    foldr (\expr acc -> acc ++ (freeVars b expr)) [] exprs

mapProgram f (Program defns) = Program (map f defns)

mapBindings f = map (\(name, expr) -> (name, f expr))
