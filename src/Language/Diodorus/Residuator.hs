module Language.Diodorus.Residuator where

import Language.Diodorus.Model
import qualified Language.Diodorus.Env as Env

import qualified Language.Diodorus.Eval as Eval

data KnownStatus = Known Value
                 | Unknown
    deriving (Show, Ord, Eq)

type KEnv = Env.Env Name KnownStatus


residuateExpr :: KEnv -> KEnv -> Expr -> Expr
residuateExpr globals env (Apply e es) = error "not implemented"
residuateExpr globals env e@(Name n) = case Env.fetch n env of
    Just (Known v) -> Lit v
    _ -> case Env.fetch n globals of
        Just (Known gv) -> Lit gv
        _ -> e
residuateExpr globals env (Eval e) = error "not implemented"
residuateExpr globals env (Lit (Fun formals body _)) = error "not implemented"
residuateExpr globals env other = other

-- All globals are known.

makeInitialEnv p = Env.map (\v -> Known v) $ Eval.makeInitialEnv p
