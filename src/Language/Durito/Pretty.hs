module Language.Durito.Pretty where

import Language.Durito.Model
import qualified Language.Durito.Env as Env


renderExpr :: Expr -> String
renderExpr (Apply e es) =
    renderExpr e ++ "(" ++ (renderExprList ", " es) ++ ")"
renderExpr (Name n) = n
renderExpr (Eval e) = "eval " ++ renderExpr e
renderExpr (Lit v) = renderValue v

renderExprList sep [] = ""
renderExprList sep [e] = renderExpr e
renderExprList sep (e:es) = (renderExpr e) ++ sep ++ (renderExprList sep es)

renderValue (Fun args body _) = "fun(" ++ (renderExprList ", " (map (\n -> Name n) args)) ++ ") -> " ++ renderExpr body
renderValue (Quote e) = "<<" ++ (renderExpr e) ++ ">>"
renderValue (Int i) = (show i)
renderValue (Builtin b) = (show b)
