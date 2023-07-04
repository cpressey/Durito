module Language.Durito.Pretty where

import Language.Durito.Model
import qualified Language.Durito.Env as Env


renderProgram :: Program -> String
renderProgram (Program []) = ""
renderProgram (Program ((name, expr):rest)) =
    "def " ++ name ++ " = " ++ (renderExpr expr) ++ "\n" ++ renderProgram (Program rest)

renderExpr :: Expr -> String
renderExpr (Apply e es) =
    renderApplier e ++ "(" ++ (renderExprList ", " es) ++ ")"
    where
        renderApplier (Name n) = n
        renderApplier (Lit v@(Builtin _)) = renderValue v
        renderApplier other = "(" ++ (renderExpr other) ++ ")"
renderExpr (Name n) = n
renderExpr (Eval e) = "eval " ++ renderExpr e
renderExpr (Lit v) = renderValue v

renderExprList sep [] = ""
renderExprList sep [e] = renderExpr e
renderExprList sep (e:es) = (renderExpr e) ++ sep ++ (renderExprList sep es)

renderValue (Fun args body _) = "fun(" ++ (renderExprList ", " (map (\n -> Name n) args)) ++ ") -> " ++ renderExpr body
renderValue (Quote e) = "<<" ++ (renderExpr e) ++ ">>"
renderValue (Int i) = (show i)
renderValue (Builtin Add) = "add"
renderValue (Builtin Mul) = "mul"
-- renderValue (Builtin b) = show b
