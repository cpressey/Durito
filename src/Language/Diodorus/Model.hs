module Language.Diodorus.Model where

type Name = String

data Program = Program [(Name, Expr)]
    deriving (Show, Ord, Eq)

data Expr = Apply Expr [Expr]
          | Name Name
          | Eval Expr
          | Lit Value
    deriving (Show, Ord, Eq)

data Value = Fun [Name] Expr
           | Quote Expr
           | Int Integer
           | Builtin Builtin
    deriving (Show, Ord, Eq)

data Builtin = Add | Mul
    deriving (Show, Ord, Eq)
