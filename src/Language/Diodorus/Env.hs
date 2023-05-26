module Language.Diodorus.Env where

import qualified Data.Map as Map

import Language.Diodorus.Model


data Env = Env {
    table :: Map.Map String Value
  } deriving (Show, Ord, Eq)


empty = Env{ table=Map.empty }
fetch k env = Map.lookup k (table env)
insert k v env = env{ table=Map.insert k v (table env) }
update f k env = env{ table=Map.update f k (table env) }
