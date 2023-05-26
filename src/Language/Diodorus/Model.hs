module Language.Diodorus.Model where

import Language.Diodorus.Env

type Name = String

type DEnv = Env Name Value


data Program = Program [(Name, Expr)]
    deriving (Show, Ord, Eq)

data Expr = Apply Expr [Expr]
          | Name Name
          | Eval Expr
          | Lit Value
    deriving (Show, Ord, Eq)

data Value = Fun [Name] Expr DEnv
           | Quote Expr
           | Int Integer
           | Builtin Builtin
    deriving (Show, Ord, Eq)

data Builtin = Add | Mul
    deriving (Show, Ord, Eq)
