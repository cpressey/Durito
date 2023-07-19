module Language.Durito.Pretty where

import Language.Durito.Model
import Language.Durito.Builtins
import qualified Language.Durito.Env as Env


renderProgram :: Program -> String
renderProgram (Program []) = ""
renderProgram (Program ((name, value):rest)) =
    "def " ++ name ++ " = " ++ (renderValue value) ++ "\n" ++ renderProgram (Program rest)

renderExpr :: Expr -> String
renderExpr (Apply e es) =
    renderApplier e ++ "(" ++ (renderExprList ", " es) ++ ")"
    where
        renderApplier (Name n) = n
        renderApplier (Lit v@(Builtin _)) = renderValue v
        renderApplier other = "(" ++ (renderExpr other) ++ ")"
renderExpr (Let [] expr) = renderExpr expr
renderExpr (Let ((n,e):bindings) expr) =
    "let " ++ n ++ " = " ++ (renderExpr e) ++ " in " ++ (renderExpr $ Let bindings expr)
renderExpr (Name n) = n
renderExpr (Lit v) = renderValue v

renderExprList sep [] = ""
renderExprList sep [e] = renderExpr e
renderExprList sep (e:es) = (renderExpr e) ++ sep ++ (renderExprList sep es)

renderValue (Fun args body _) = "fun(" ++ (renderExprList ", " (map (\n -> Name n) args)) ++ ") -> " ++ renderExpr body
renderValue (Quote e) = "<<" ++ (renderExpr e) ++ ">>"
renderValue (Int i) = (show i)
renderValue (Cons h t) = "[" ++ (renderValue h) ++ (renderListTail t)
renderValue (Nil) = "[]"
renderValue (Builtin bi) = renderBuiltin bi

renderListTail Nil = "]"
renderListTail (Cons h t) = ", " ++ (renderValue h) ++ (renderListTail t)
renderListTail t = " | " ++ (renderValue t) ++ "]"

renderBindings [] = ""
renderBindings [(name, expr)] = renderBinding name expr
renderBindings ((name, expr):bindings) = (renderBinding name expr) ++ ", " ++ renderBindings bindings

renderBinding name expr = name ++ " -> " ++ renderExpr expr
