module Parser where

import Data.Monoid

import Language.Haskell.Exts
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data
import ControlFlow

data Environment = Env 
    { types :: TyEnv 
    , usedVarNames :: [VName] 
    , usedLabels :: [Label] 
    , usedTypeNames :: [TName] 
    }

data Records = Constraints
    { fpCons :: FpCons
    , tyCons :: TyCons
    , srcLabel :: [(Label, SrcLoc)] 
    }

instance Monoid Records where 
    mempty = Constraints { fpCons = mempty 
                  , tyCons = mempty
                  , srcLabel = mempty
                  }

    mappend x y = Constraints { fpCons = fpCons x `mappend` fpCons y
                       , tyCons = tyCons x `mappend` tyCons y
                       , srcLabel = srcLabel x `mappend` srcLabel y
                       }

type Result = ReaderT Environment (WriterT Records IO)
type TypedProgram = [(SrcLoc, Data.Type)]

parser :: String -> Result TypedProgram
parser filename = do
    parsedModuleWithIO <- lift $ lift $ parseFile filename
    let parsedModule = fromParseResult parsedModuleWithIO
    case parsedModule of
        Module _ name _ _ _ _ decls -> parseDecls decls

serializedParse :: [a] -> (a -> Result TypedProgram) -> Result TypedProgram
serializedParse [] f = return mempty
serializedParse (x:xs) f = (liftM2 mappend) (f x) (serializedParse xs f)

parseDecls :: [Decl] -> Result TypedProgram
parseDecls decls = serializedParse decls parseDecl

parseDecl :: Decl -> Result TypedProgram
parseDecl = undefined
