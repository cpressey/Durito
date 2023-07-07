module Language.Durito.Residuator where

--import Debug.Trace

import Language.Durito.Model
import qualified Language.Durito.Env as Env

import qualified Language.Durito.Eval as Eval

--
-- A "KEnv" maps names to the knowledge
-- about values ahead-of-time (static analysis).
-- A name is either known (in which case we have its value)
-- or it is not present in the map.
--

data KnownStatus = Known Value
    deriving (Show, Ord, Eq)

type KEnv = Env.Env Name KnownStatus

makeKnown :: VEnv -> KEnv
makeKnown venv = Env.map (\v -> Known v) venv

extractKnown :: KEnv -> VEnv
extractKnown kenv = Env.map (\(Known v) -> v) kenv

isKnown :: KEnv -> Expr -> Bool
isKnown globals expr@(Lit (Fun formals body _)) =
    -- A function value is known if it has no free variables.
    -- (Even better would be: if all its free variables are known...)
    let
        b = Env.keys globals
        fv = freeVars b expr
    in
        fv == []
isKnown _ (Lit _) = True
isKnown _ _ = False

--
-- Residuate expressions.
--
residuateExpr :: KEnv -> KEnv -> Expr -> Expr

--
-- Residuate a function application.
--
residuateExpr globals env orig@(Apply e es) =
    let
        residuatedE = residuateExpr globals env e
        residuatedArgs = map (residuateExpr globals env) es
        eKnown = isKnown globals residuatedE
        argsKnown = all (isKnown globals) residuatedArgs
        newExpr = Apply residuatedE residuatedArgs
    in case (eKnown, argsKnown) of
        (True, True) ->
            --traceShow newExpr
                Lit $ Eval.evalExpr (extractKnown globals) (extractKnown env) newExpr
        _ ->
            newExpr

--
-- Residuate a usage of a name.
--
residuateExpr globals env orig@(Name n) = case Env.fetch n env of
    Just (Known v) -> Lit v
    _ -> case Env.fetch n globals of
        Just (Known v) -> Lit v
        _ -> orig

residuateExpr globals env (Lit lit) = Lit $ residuateLit globals env lit

--
-- Residuate a literal function.
-- When we residuate a literal function, we install in it the current environment.
--
-- And when descending into function literals, we
-- extend the known-env with the formals as unknowns
--
residuateLit :: KEnv -> KEnv -> Value -> Value
residuateLit globals env (Fun formals body valueEnv) =
    let
        knownEnv = makeKnown valueEnv
        knownEnv' = Env.union env knownEnv

        -- Mark all the formals as "unknown":
        -- FIXME: actually delete any formals from the lexical-known-env
        -- in case of shadowing.

        body' = residuateExpr globals knownEnv' body
        valueEnv' = extractKnown knownEnv'
    in
        Fun formals body' valueEnv'
residuateLit globals env other = other

--
-- Residuate a program.
--
residuateProgram :: Program -> Program
residuateProgram program =
    let
        globals = makeKnown $ Eval.makeInitialEnv program
        f (name, value) = (name, residuateLit globals Env.empty value)
    in
        mapProgram f program
