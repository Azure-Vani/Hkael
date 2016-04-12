module Infer where

import Text.Groom
import Debug.Trace

import Data.Map as Map
import Data.Set as Set

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State

import Data
import Constraint
import Syntax
import Environment

type Infer a = (RWST
                   Env                -- Type Environment
                   [TypedProgram]     -- Parse Result
                   InferState         -- Inference State
                   (Except TypeError) -- Inference Errors
                   a)                 -- Result

data InferState = InferState { tyCount :: Int
                             , fpCount :: Int
                             }

lettersTy :: [String]
lettersTy = [1..] >>= flip replicateM ['a' .. 'z']

lettersFp :: [String]
lettersFp = [1..] >>= flip replicateM ['A' .. 'Z']

freshFpvar :: Infer FP
freshFpvar = do
    s <- get
    put s{fpCount = fpCount s + 1}
    return $ FPVar $ LVar (lettersFp !! fpCount s)

freshTyvar :: Infer Type
freshTyvar = do
    l <- freshFpvar
    s <- get
    put s{tyCount = tyCount s + 1}
    return $ TyVar (TVar (lettersTy !! tyCount s)) l

instantiate :: TypeScm -> Infer (Type, [FConstraint])
instantiate (TyForall as ty) = do
    as' <- mapM (\_ -> freshTyvar) as
    let s = Subst $ Map.fromList $ zip as as'
    instantiateFp $ apply s ty

instantiateFp :: FlppScm -> Infer (Type, [FConstraint])
instantiateFp (FpForall as c ty) = do
    as' <- mapM (\_ -> freshFpvar) as
    let s = Subst $ Map.fromList $ zip as as'
    return (apply s ty, apply s c ++ zipWith (\x y -> 
        (typeDummy x, typeDummy $ FPVar y)) as' as)

generalize :: Type -> [FConstraint] -> Infer TypeScm
generalize ty cs = do
    ty' <- generalizeFp ty cs
    e <- ask
    let as =  Set.toList $ fv ty' `Set.difference` fv e
    return $ TyForall as ty'

generalizeFp :: Type -> [FConstraint] -> Infer FlppScm
generalizeFp ty c = do
    e <- ask
    let as = Set.toList $ fv ty `Set.union` fv c `Set.difference` fv e
    return $ FpForall as c ty 

lookupEnv :: Name -> Infer (Type, [FConstraint])
lookupEnv name = do
    e <- ask
    case Environment.lookup e name of
        Just t -> instantiate t
        Nothing -> throwError $ UnboundVariables name

inEnv :: (Name, TypeScm) -> Infer a -> Infer a
inEnv (v, t) m = do
    let scope e = (remove e v) `extend` (v, t) 
    local scope m

infer :: Expr -> Infer (Type, [TConstraint], [FConstraint])
infer expr = case expr of
    Lit loc (LInt _) -> do
        let t = typeInt (fpSingleton loc)
        tell [(loc, t)]
        return (t, [], [])

    Lit loc (LBool _) -> do
        let t = typeBool (fpSingleton loc)
        tell [(loc, t)]
        return (t, [], [])

    Var loc name -> do
        (t, c) <- lookupEnv name 
        tell [(loc, t)]
        return (t, [], c)

    App loc e1 e2 -> do
        (t1, d1, c1) <- infer e1
        (t2, d2, c2) <- infer e2
        tv <- freshTyvar
        fp <- freshFpvar
        tell [(loc, tv)]
        return (tv, d1 ++ d2 ++ [(t1, TyArr t2 tv fp)],
                    c1 ++ c2 ++ [(t1, TyArr t2 tv fp)])

    Lambda loc name e -> do 
       a <- freshTyvar
       (t, d, c) <- inEnv (name, TyForall [] (FpForall [] [] a)) (infer e)
       let tv = TyArr a t (fpSingleton loc)
       tell [(loc, tv)]
       return (tv, d, c)

    Let loc name e1 e2 -> do
        (t, d, c) <- infer e1
        case runSolve d of
            Left error -> throwError error
            Right sub -> do
                t' <- local (apply sub) generalize t c
                (tv, d', c') <- inEnv (name, t') $ local (apply sub) (infer e2)
                tell [(loc, tv)]
                return (tv, d' ++ d, c' ++ c)
    
    If loc e1 e2 e3 -> do
        t  <- freshTyvar
        l  <- freshFpvar
        (t1, d1, c1) <- infer e1
        (t2, d2, c2) <- infer e2
        (t3, d3, c3) <- infer e3
        let d' = [(t1, typeBool l), (t2, t), (t3, t)]
        let c' = [(t2, t), (t3, t)]
        tell [(loc, t)]
        return (t, d1 ++ d2 ++ d3 ++ d', c1 ++ c2 ++ c3 ++ c')

    Fix loc name e -> do
        tv <- freshTyvar
        (t, d, c)  <- inEnv (name, TyForall [] (FpForall [] [] tv)) (infer e)
        tell [(loc, t)]
        return (t, (tv, t):d, (t, tv):c)

    Op loc op e1 e2 -> do
        (t1, d1, c1) <- infer e1
        (t2, d2, c2) <- infer e2
        l <- freshFpvar
        let (d, tv) = case op of 
                Add -> ([(t1, typeInt l), (t2, typeInt l)], typeInt l)
                Sub -> ([(t1, typeInt l), (t2, typeInt l)], typeInt l)
                Mul -> ([(t1, typeInt l), (t2, typeInt l)], typeInt l)
                Eql -> ([(t1, typeInt l), (t2, typeInt l)], typeBool l)
        return (tv, d1 ++ d2 ++ d, c1 ++ c2)

-- Run
initInfer :: InferState
initInfer = InferState { tyCount = 0, fpCount = 0 }

runInfer :: Env -> Infer a -> Either TypeError (a, [TypedProgram])
runInfer env m = runExcept $ evalRWST m env initInfer

inferExpr :: Expr -> Either TypeError (Type, [TypedProgram])
inferExpr expr = do
    let initEnv = emptyEnv
    ((ty, d, c), typedProgram) <- runInfer initEnv (infer expr)
    sub <- Constraint.runSolve d
    let c' = apply sub c
    relation <- Constraint.runGen c'
    let fpsub = Constraint.accumulate relation
    return (apply fpsub $ apply sub ty, apply fpsub $ apply sub typedProgram)

