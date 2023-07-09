module Language.Durito.BuiltinType where

--
-- Ideally, these should really be defined in Language.Durito.Builtins,
-- but to avoid circularity, they have been placed in their own module.
--

data Builtin = DuritoAdd
             | DuritoMul
             | DuritoEval
             | DuritoCons
    deriving (Show, Ord, Eq)
