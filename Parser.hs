{-# LANGUAGE TypeOperators #-}
module Parser where

import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Language.Haskell.Exts (parseFile, fromParseResult)
import Language.Haskell.Exts.Syntax hiding (Type, TyFun, TyTuple, TyList, TyApp, TyVar, TyCon)

import Data
import Environments
import Utils
import Constraints

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

parseAlt :: Type -> Alt -> Result Type
parseAlt = undefined

atomVariable :: QName -> Result Type
atomVariable qname = do
    env <- get
    label <- lift getLabel
    let ty = lookupType (extractQName qname) env
    lift $ trackType ty
    return ty

parseExp :: Exp -> Result Type
parseExp exp = case exp of
    Var qname -> atomVariable qname
    Con qname -> atomVariable qname

    Lit lit -> do
        label <- lift getLabel
        let fp = atomfp label
        let ty = case lit of
                Char c    -> TyCon tyString fp
                Int  i    -> TyCon tyInt    fp
                String st -> TyCon tyString fp
        lift $ trackType ty
        return ty

    InfixApp exp1 op exp2 -> do
        -- add constraint ty1 -> ty2 -> ty = type(op)
        ty1 <- parseExp exp1
        let opname = case op of 
                QVarOp qname -> qname
                QConOp qname -> qname
        tyop <- atomVariable opname
        ty2  <- parseExp exp2
        ty   <- lift $ getType
        t'   <- lift $ makeFunTy [ty1, ty2, ty] Nothing
        lift $ lift $ constrain t' tyop
        lift $ trackType ty
        return ty

    App exp1 exp2 -> do
        -- add constraint ty2 -> ty = type(exp1)
        ty1 <- parseExp exp1
        ty2 <- parseExp exp2
        ty  <- lift $ getType   
        x   <- lift $ makeFunTy [ty2, ty] Nothing
        lift $ lift $ constrain x ty1
        lift $ trackType ty
        return ty

    Lambda srcLoc pats exp -> do
        env <- get
        argTys <- mapM parsePat pats
        bodyTy <- parseExp exp
        label <- lift $ getLabel
        let fp = atomfp label
        ty <- lift $ makeFunTy (argTys ++ [bodyTy]) Nothing
        put env
        lift $ trackType ty
        return ty
        
    Let binds exp -> do
        env <- get
        parseBinds binds
        ty <- parseExp exp
        put env
        lift $ trackType ty
        return ty

    If condition thenExp elseExp -> do
        condType <- parseExp condition
        thenType <- parseExp thenExp
        elseType <- parseExp elseExp
        lname    <- lift $ getLabelName 
        lift $ lift $ constrain condType (TyCon tyBool lname)
        lift $ lift $ constrain thenType elseType
        lift $ trackType thenType
        return thenType

    Case exp alts -> do
        expType <- parseExp exp
        altTypes <- mapM (parseAlt expType) alts
        lift $ lift $ mergeM altTypes constrain
        lift $ trackType $ head altTypes
        return $ head altTypes

    Do stmts -> undefined

    Tuple boxed exps -> do
        expTypes <- mapM parseExp exps
        label <- lift getLabel
        let ty = TyTuple expTypes (atomfp label)
        lift $ trackType ty
        return ty

    List exps -> do 
        expTypes <- mapM parseExp exps
        label <- lift getLabel
        let ty = TyList (head expTypes) (atomfp label)
        lift $ trackType ty
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
        funcTy <- lift $ makeFunTy (argTys ++ [bodyTy]) Nothing
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

