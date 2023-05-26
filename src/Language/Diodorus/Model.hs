module Language.Diodorus.Model where

data Expr = Apply Expr [Expr]
          | Name String
          | Eval Expr
          | LitFun [String] Expr
          | LitQuote Expr
          | LitInt Integer
    deriving (Show, Ord, Eq)
