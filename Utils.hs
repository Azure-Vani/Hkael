module Utils where

import Language.Haskell.Exts hiding (Type)

import Data

extractName :: Name -> String
extractName x = case x of
    Ident name  -> name
    Symbol name -> name

makeFunTy :: [Type] -> Type
makeFunTy = undefined

makeVName :: SrcLoc -> String -> VName
makeVName = (,)
