{-# LANGUAGE TypeOperators #-}
module Parser where

import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Language.Haskell.Exts (parseFile, fromParseResult)
import Language.Haskell.Exts.Syntax hiding (Type)

import Data
import Utils
import ControlFlow

data Environment = Env
    { types :: TyEnv
    }

data Used = Used
    { usedTypeNames :: TName
    , usedLabel     :: Label
    }

data Records = Constraints
    { fpCons    :: FpCons
    , tyCons    :: TyCons
    , typedProg :: TypedProg
    }

instance Monoid Records where
    mempty = Constraints { fpCons = mempty
                  , tyCons = mempty
                  , typedProg = mempty
                  }

    mappend x y = Constraints { fpCons = fpCons x `mappend` fpCons y
                       , tyCons = tyCons x `mappend` tyCons y
                       , typedProg = typedProg x `mappend` typedProg y
                       }

type RecordsM = WriterT Records IO
type UsedM    = StateT Used RecordsM
type Result   = StateT Environment UsedM

-- Environment operations
addType :: VName -> Type -> Environment -> Environment
addType name ty env = Env $ (:) ((,) name ty) (types env)

lookupType :: VName -> Environment -> Type
lookupType name env = fromJust $ lookup name (types env)

-- Used operations
renewLabel :: Used -> (Label, Used)
renewLabel used = (oldLable, used { usedLabel = oldLable + 1 })
    where oldLable = usedLabel used

renewTypeName :: Used -> (TName, Used)
renewTypeName used = (oldTypeName, used { usedTypeNames = oldTypeName + 1 }) 
    where oldTypeName = usedTypeNames used

getLabel :: UsedM Label
getLabel = do
    used <- get
    let (label, renewUsed) = renewLabel used
    put renewUsed
    return label

getTypeName :: UsedM Label
getTypeName = do
    used <- get
    let (label, renewUsed) = renewLabel used
    put renewUsed
    return label

-- Parsers
serializedParse :: Monoid b => [a] -> (a -> Result b) -> Result b
serializedParse [] f     = return mempty
serializedParse (x:xs) f = (liftM2 mappend) (f x) (serializedParse xs f)

parser :: FilePath -> Result ()
parser filename = do
    parsedModuleWithIO <- lift $ lift $ lift $ parseFile filename
    let parsedModule = fromParseResult parsedModuleWithIO
    case parsedModule of
        Module _ name _ _ _ _ decls -> serializedParse decls parseDecl

parsePat :: Pat -> Result Type
parsePat = undefined

parseBinds :: Binds -> Result ()
parseBinds bind = case bind of
    BDecls decls -> serializedParse decls parseDecl
    IPBinds _    -> undefined

parseStmt :: Stmt -> Result ()
parseStmt = undefined

parseExp :: Exp -> Result Type
parseExp exp = case exp of
    Var qName -> do
        env <- get
        label <- lift getLabel
        let ty = lookupType (extractQName qName) env
        lift $ lift $ tell $ Constraints {fpCons = mempty, tyCons = mempty, typedProg = [(label, ty)]}
        return ty

parseRhs :: Rhs -> Result Type
parseRhs rhs = case rhs of
    UnGuardedRhs exp -> parseExp exp
    GuardedRhss guardedRhss -> do
        tys <- mapM parseGuardedRhs guardedRhss
        return $ head tys

parseGuardedRhs :: GuardedRhs -> Result Type
parseGuardedRhs guardedRhs = case guardedRhs of
    GuardedRhs srcLoc stmts exp -> do
        serializedParse stmts parseStmt
        parseExp exp

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
        let funcName = extractName name
        let funcTy = makeFunTy (argTys ++ [bodyTy])
        -- recovery env and add this function to env
        put $ addType funcName funcTy env
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

