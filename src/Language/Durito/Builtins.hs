module Language.Durito.Builtins where

import Language.Durito.BuiltinType
import qualified Language.Durito.Env as Env
import Language.Durito.Model


renderBuiltin DuritoAdd = "add"
renderBuiltin DuritoMul = "mul"
renderBuiltin DuritoEval = "eval"
renderBuiltin DuritoCons = "cons"


evalBuiltin _ DuritoAdd [(Int x), (Int y)] =
    Int (x + y)
evalBuiltin _ DuritoMul [(Int x), (Int y)] =
    Int (x * y)
evalBuiltin _ DuritoCons [x, y] =
    Cons x y
evalBuiltin evaluator DuritoEval [(Quote qe)] =
    evaluator qe
evalBuiltin _ other args =
    error $ "type mismatch: " ++ (show other) ++ " " ++ (show args)

builtinsEnv = Env.extend Env.empty
    ["mul", "add", "eval", "cons", "nil"]
    [Builtin DuritoMul, Builtin DuritoAdd, Builtin DuritoEval, Builtin DuritoCons, Nil]
