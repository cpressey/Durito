module Language.Diodorus.Residuator where

--import Debug.Trace

import Language.Diodorus.Model
import qualified Language.Diodorus.Env as Env

import qualified Language.Diodorus.Eval as Eval

data KnownStatus = Known Value
                 | Unknown
    deriving (Show, Ord, Eq)

type KEnv = Env.Env Name KnownStatus


isKnown :: Expr -> Bool
isKnown (Lit (Fun formals body env)) = Env.isEmpty env
isKnown (Lit _) = True
isKnown _ = False


residuateExpr :: KEnv -> KEnv -> Expr -> Expr
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
            orig

residuateExpr globals env orig@(Name n) = case Env.fetch n env of
    Just (Known v) -> Lit v
    _ -> case Env.fetch n globals of
        Just (Known v) -> Lit v
        _ -> orig

residuateExpr globals env orig@(Eval e) =
    let
        residuatedE = residuateExpr globals env e
        eKnown = isKnown residuatedE
    in case eKnown of
        True ->
            Lit $ Eval.evalExpr (Env.map (\(Known v) -> v) globals) Env.empty orig
        _ ->
            orig

-- When we residuate a literal function, we install in it the current environment.
residuateExpr globals env (Lit (Fun formals body _)) =
    Lit (Fun formals body (Env.map (\(Known v) -> v) env))

residuateExpr globals env other = other

-- When descending into function literals, we
-- extend the known-env with the formals as unknowns

residuateFunDefn :: KEnv -> KEnv -> Value -> Expr
residuateFunDefn globals env (Fun formals body lexicalEnv) =
    let
        actuals = map (\_ -> Unknown) formals
        lexicalKnownEnv = Env.map (\v -> Known v) lexicalEnv
        env' = Env.extend lexicalKnownEnv formals actuals
        result = residuateExpr globals env' body
    in
        result

-- All globals are known.

makeInitialEnv p = Env.map (\v -> Known v) $ Eval.makeInitialEnv p
