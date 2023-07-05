module Language.Durito.Residuator where

--import Debug.Trace

import Language.Durito.Model
import qualified Language.Durito.Env as Env

import qualified Language.Durito.Eval as Eval

data KnownStatus = Known Value
                 | Unknown
    deriving (Show, Ord, Eq)

type KEnv = Env.Env Name KnownStatus


isKnown :: Expr -> Bool
isKnown (Lit (Fun formals body env)) = Env.isEmpty env
isKnown (Lit _) = True
isKnown _ = False


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
    in case (eKnown, argsKnown) of
        (True, True) ->
            Lit $ Eval.evalExpr (Env.map (\(Known v) -> v) globals) Env.empty orig
        _ ->
            Apply residuatedE residuatedArgs

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
    in case exprKnown of
        True ->
            Lit $ Eval.evalExpr (Env.map (\(Known v) -> v) globals) Env.empty orig
        _ ->
            Eval residuatedExpr

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
    in case (exprKnown, bindingsKnown) of
        (True, True) ->
            Lit $ Eval.evalExpr (Env.map (\(Known v) -> v) globals) Env.empty orig
        _ ->
            Subst residuatedBindings residuatedExpr

residuateExpr globals env (Lit lit) = Lit $ residuateLit globals env lit

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
residuateLit globals env (Fun formals body lexicalEnv) =
    let
        installedEnv = (Env.map (\(Known v) -> v) env)
        lexicalEnv' = Env.union installedEnv lexicalEnv
        actuals = map (\_ -> Unknown) formals
        lexicalKnownEnv = Env.map (\v -> Known v) lexicalEnv'
        env' = Env.extend lexicalKnownEnv formals actuals
        body' = residuateExpr globals env' body
    in
        Fun formals body' lexicalEnv'
residuateLit globals env other = other

--
-- Residuate PROGRAMS
--

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
