module Environment where

import Data.Map as Map

import Syntax
import Data

data Env = Env { types :: Map Name TypeScm }

extend :: Env -> (Name, TypeScm) -> Env
extend (Env e) (v, t) = Env $ Map.insert v t e

remove :: Env -> Name -> Env
remove (Env e) v = Env $ Map.delete v e

singleton :: Name -> TypeScm -> Env
singleton v t = Env $ Map.singleton v t

lookup :: Env -> Name -> Maybe TypeScm
lookup (Env e) x  = Map.lookup x e

merge :: Env -> Env -> Env
merge (Env e1) (Env e2) = Env $ Map.union e1 e1

emptyEnv :: Env
emptyEnv = Env Map.empty
