module Parser where

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Language.Haskell.Exts (parseFile, fromParseResult)
import Language.Haskell.Exts.Syntax hiding (Type)

import Data
import Utils
import ControlFlow

type TypedProg= [(SrcLoc, Type)]

data Environment = Env
    { types :: TyEnv
    }

addTypeEnv :: VName -> Type -> Environment -> Environment
addTypeEnv name ty env = Env $ (:) ((,) name ty) (types env)

data Records = Constraints
    { fpCons :: FpCons
    , tyCons :: TyCons
    , typedProg :: TypedProg
    , usedTypeNames :: [TName]
    }

instance Monoid Records where
    mempty = Constraints { fpCons = mempty
                  , tyCons = mempty
                  , typedProg = mempty
                  , usedTypeNames = mempty
                  }

    mappend x y = Constraints { fpCons = fpCons x `mappend` fpCons y
                       , tyCons = tyCons x `mappend` tyCons y
                       , typedProg = typedProg x `mappend` typedProg y
                       , usedTypeNames = usedTypeNames x `mappend` usedTypeNames y
                       }

type Result = StateT Environment (WriterT Records IO)

updateEnvTypes :: Environment -> TyEnv -> Environment
updateEnvTypes = undefined

parsePat :: Pat -> Result Type
parsePat = undefined

parser :: FilePath -> Result ()
parser filename = do
    parsedModuleWithIO <- lift $ lift $ parseFile filename
    let parsedModule = fromParseResult parsedModuleWithIO
    case parsedModule of
        Module _ name _ _ _ _ decls -> parseDecls decls

serializedParse :: Monoid b => [a] -> (a -> Result b) -> Result b
serializedParse [] f     = return mempty
serializedParse (x:xs) f = (liftM2 mappend) (f x) (serializedParse xs f)

parseDecls :: [Decl] -> Result ()
parseDecls decls = serializedParse decls parseDecl

parseBinds :: Binds -> Result ()
parseBinds = undefined

parseRhs :: Rhs -> Result Type
parseRhs   = undefined

parseMatch :: Match -> Result ()
parseMatch match = case match of
    Match srcLoc name pats Nothing rhs binds -> do
        -- get current env
        env <- get
        -- parse all arguments one by one
        argTys <- mapM parsePat pats
        -- add variables in `where` clause
        case binds of
            Just x -> parseBinds x
            Nothing -> return ()
        -- parse function body
        bodyTy <- parseRhs rhs
        let funcName = makeVName srcLoc $ extractName name
        let funcTy = makeFunTy (argTys ++ [bodyTy])
        -- recovery env and add this function to env
        put $ addTypeEnv funcName funcTy env
        return  ()

parseDecl :: Decl -> Result ()
parseDecl decl = case decl of
    TypeDecl  srcLoc name tyVars ty                            -> undefined
    DataDecl  srcLoc dataorNew context name tyVars _ _         -> undefined
    ClassDecl srcLoc context name tyVars funDeps classDecl     -> undefined
    InstDecl  srcLoc overlap tyVars context name tys instDecls -> undefined
    TypeSig   srcLoc names ty                                  -> undefined
    FunBind   matches                                          -> serializedParse matches parseMatch
    PatBind   srcLoc pat rhs binds                             -> do
        -- add arguments to current env
        parsePat pat
        -- add variables in `where` clause
        case binds of
            Just x  -> parseBinds x
            Nothing -> return ()
        parseRhs rhs
        return ()

