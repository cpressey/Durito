module Language.Durito.Env where

import qualified Data.Map as Map


data Env tk tv = Env {
    table :: Map.Map tk tv
  } deriving (Show, Ord, Eq)


empty = Env{ table=Map.empty }
fetch k env = Map.lookup k (table env)
insert k v env = env{ table=Map.insert k v (table env) }
update f k env = env{ table=Map.update f k (table env) }

isEmpty env = Map.null (table env)

extend :: Ord tk => Env tk tv -> [tk] -> [tv] -> Env tk tv
extend env [] [] = env
extend env (formal:formals) (actual:actuals) =
    extend (insert formal actual env) formals actuals

map :: Ord tk => (tv1 -> tv2) -> Env tk tv1 -> Env tk tv2
map f env = env{ table=Map.map f (table env) }

foldrWithKey :: (tk -> tv -> ta -> ta) -> ta -> Env tk tv -> ta
foldrWithKey f acc0 env = Map.foldrWithKey f acc0 (table env)

union :: Ord tk => Env tk tv -> Env tk tv -> Env tk tv
union env1 env2 = Env { table=Map.union (table env1) (table env2) }
