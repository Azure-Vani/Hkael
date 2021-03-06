module Utils where

import Language.Haskell.Exts hiding (Type)

import Data

extractName :: Name -> VName
extractName x = case x of
    Ident name  -> name
    Symbol name -> name

extractQName :: QName -> VName
extractQName x = case x of
    Qual _ name -> extractName name
    UnQual name ->  extractName name
    Special sp -> case sp of
        UnitCon          -> "unit"
        Cons             -> "cons"
        ListCon          -> "list"
        FunCon           -> "fun"
        TupleCon boxed x -> "tuple" ++ show x

reduceM :: Monad m => [a] -> (a -> a -> m a) -> m a
reduceM = undefined

mergeM :: Monad m => [a] -> (a -> a -> m ()) -> m ()
mergeM = undefined
