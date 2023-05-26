module Language.Diodorus.Env where

import qualified Data.Map as Map


data Env tk tv = Env {
    table :: Map.Map tk tv
  } deriving (Show, Ord, Eq)


empty = Env{ table=Map.empty }
fetch k env = Map.lookup k (table env)
insert k v env = env{ table=Map.insert k v (table env) }
update f k env = env{ table=Map.update f k (table env) }


extend :: Ord tk => Env tk tv -> [tk] -> [tv] -> Env tk tv
extend env [] [] = env
extend env (formal:formals) (actual:actuals) =
    extend (insert formal actual env) formals actuals
