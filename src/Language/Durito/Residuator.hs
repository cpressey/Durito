module Language.Durito.Residuator where

--import Debug.Trace

import Language.Durito.Model
import qualified Language.Durito.Env as Env

import qualified Language.Durito.Eval as Eval


isKnown :: Expr -> Bool
-- FIXME look through the body and see if there are any identifiers that are not in KEnv
isKnown (Lit (Fun formals body env kenv)) = Env.isEmpty env
isKnown (Lit _) = True
isKnown _ = False


extractKnown :: KEnv -> VEnv
extractKnown kenv = Env.map (\(Known v) -> v) kenv


--
-- Residuate EXPRESSIONS
--

residuateExpr :: KEnv -> KEnv -> Expr -> Expr

--
-- Residuate a function application.
--
residuateExpr globals env orig@(Apply e es) =
    let
        residuatedE = residuateExpr globals env e
        residuatedArgs = map (residuateExpr globals env) es
        eKnown = isKnown residuatedE
        argsKnown = all (isKnown) residuatedArgs
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

--
-- Residuate an `eval`.
--
residuateExpr globals env orig@(Eval expr) =
    let
        residuatedExpr = residuateExpr globals env expr
        exprKnown = isKnown residuatedExpr
        newExpr = Eval residuatedExpr
    in case exprKnown of
        True ->
            --traceShow newExpr
                Lit $ Eval.evalExpr (extractKnown globals) (extractKnown env) newExpr
        _ ->
            newExpr

--
-- Residuate an `subst`.
-- Ideally this should just be a function application!
-- But we'd need lists and such for that
--
residuateExpr globals env orig@(Subst bindings expr) =
    let
        residuatedExpr = residuateExpr globals env expr
        residuatedBindings = residuateBindings globals env bindings
        exprKnown = isKnown residuatedExpr
        bindingsKnown = all (isKnown) (map (snd) residuatedBindings)
        newExpr = Subst residuatedBindings residuatedExpr
    in case (exprKnown, bindingsKnown) of
        (True, True) ->
            --traceShow newExpr
                Lit $ Eval.evalExpr (extractKnown globals) (extractKnown env) newExpr
        _ ->
            newExpr

residuateExpr globals env (Lit lit) = Lit $ residuateLit globals env lit

residuateBindings :: KEnv -> KEnv -> [(Name, Expr)] -> [(Name, Expr)]
residuateBindings globals env [] = []
residuateBindings globals env ((name, expr):rest) =
    (name, residuateExpr globals env expr):residuateBindings globals env rest

--
-- Residuate a literal function.
-- When we residuate a literal function, we install in it the current environment.
--
-- And when descending into function literals, we
-- extend the known-env with the formals as unknowns
--
residuateLit :: KEnv -> KEnv -> Value -> Value
residuateLit globals env (Fun formals body valueEnv knownEnv) =
    let
        knownEnv' = Env.union env knownEnv

        -- Mark all the formals as "unknown":
        -- FIXME: actually delete any formals from the lexical-known-env
        -- in case of shadowing.

        body' = residuateExpr globals knownEnv' body
    in
        Fun formals body' (extractKnown knownEnv') knownEnv'
residuateLit globals env other = other

--
-- Residuate PROGRAMS
--

residuateProgram :: Program -> Program
residuateProgram program =
    let
        globals = makeInitialEnv program
        f (name, value) = (name, residuateLit globals Env.empty value)
    in
        mapProgram f program

--
-- Make initial known-map from globals.
-- All globals are known.
--
makeInitialEnv p = Env.map (\v -> Known v) $ Eval.makeInitialEnv p
