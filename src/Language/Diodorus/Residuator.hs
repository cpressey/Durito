module Language.Diodorus.Residuator where

import Language.Diodorus.Model
import qualified Language.Diodorus.Env as Env

import qualified Language.Diodorus.Eval as Eval

data KnownStatus = Known Value
                 | Unknown
    deriving (Show, Ord, Eq)

type KEnv = Env.Env Name KnownStatus


isKnown :: Expr -> Bool
isKnown (Lit (Fun formals body _)) = False  -- TODO: muy pessimistic!
isKnown (Lit _) = True
isKnown _ = False


residuateExpr :: KEnv -> KEnv -> Expr -> Expr
residuateExpr globals env app@(Apply e es) =
    let
        residuatedE = residuateExpr globals env e
        residuatedArgs = map (residuateExpr globals env) es
    in
        case (isKnown residuatedE, all (isKnown) residuatedArgs) of
            (True, True) ->
                let
                    value = Eval.evalExpr (Env.map (\(Known v) -> v) globals) Env.empty app
                in
                    Lit value
            _ ->
                app

residuateExpr globals env e@(Name n) = case Env.fetch n env of
    Just (Known v) -> Lit v
    _ -> case Env.fetch n globals of
        Just (Known v) -> Lit v
        _ -> e
residuateExpr globals env (Eval e) = error "not implemented: eval"
residuateExpr globals env e@(Lit (Fun formals body _)) = e
residuateExpr globals env other = other

-- All globals are known.

makeInitialEnv p = Env.map (\v -> Known v) $ Eval.makeInitialEnv p
