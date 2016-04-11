{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Constraint where

import Text.Groom
import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity

import Data
import Environment

type TypeErrorM a = ExceptT TypeError Identity a

runTypeErrorM = runIdentity . runExceptT

type Solve a = TypeErrorM a

newtype Subst a b = Subst (Map.Map a b)
        deriving (Show, Eq, Ord)

emptySubst = Subst Map.empty

(Subst s1) `compose` (Subst s2) = 
    Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

v `bind` t | Set.singleton v == fv t    = return $ emptySubst
           | occurCheck v t             = throwError $ InfinitType (show v) (show t)
           | otherwise                  = return $ Subst $ Map.singleton v t

occurCheck v t = v `Set.member` fv t

class Substitutable var value contr | var -> value where
    apply :: Subst var value -> contr -> contr
    fv    :: contr -> Set.Set var

-- Substitution for type variable
instance Substitutable TVar Type Type where
    apply _ all@(TyCon _ _)          = all
    apply sub (TyArr t1 t2 fp)       = TyArr (sub `apply` t1) (sub `apply` t2) fp
    apply sub (TyApp t1 t2 fp)       = TyApp (sub `apply` t1) (sub `apply` t2) fp
    apply (Subst s) all@(TyVar var fp)  = modifyFp (Map.findWithDefault all var s) fp

    fv (TyCon _ _)     = Set.empty 
    fv (TyArr t1 t2 _) = fv t1 `Set.union` fv t2
    fv (TyApp t1 t2 _) = fv t1 `Set.union` fv t2
    fv (TyVar var _)     = Set.singleton var

instance Substitutable TVar Type TypeScm where
    apply (Subst s) (TyForall as ty) = TyForall as $ apply s' ty
        where s' = Subst $ foldr Map.delete s as

    fv (TyForall as ty) = fv ty `Set.difference` Set.fromList as

instance Substitutable TVar Type FlppScm where
    apply sub (FpForall as cs ty) = FpForall as (apply sub cs) (apply sub ty)

    fv (FpForall as cs ty) = fv cs `Set.union` fv ty

instance Substitutable TVar Type TConstraint where
    apply sub (t1, t2) = (sub `apply` t1, sub `apply` t2)

    fv (t1, t2) = fv t1 `Set.union` fv t2

instance Substitutable TVar Type FConstraint where
    apply sub (t1, t2) = (sub `apply` t1, sub `apply` t2)

    fv (t1, t2) = fv t1 `Set.union` fv t2

instance Substitutable TVar Type Env where
    apply s (Env e) = Env $ Map.map (apply s) e

    fv (Env e) = fv $ Map.elems e

instance Substitutable TVar Type a => Substitutable TVar Type [a] where
    apply = map . apply

    fv = foldr (Set.union . fv) Set.empty

instance Substitutable TVar Type a => Substitutable TVar Type (Decompose a) where
    apply s (ArrL x) = ArrL $ apply s x
    apply s (Is x) = Is $ apply s x

    fv (ArrL x) = fv x
    fv (Is x) = fv x

-- Substitution for flow properties variable
instance Substitutable LVar FP Type where
    apply sub (TyCon name fp)        = TyCon name $ sub `apply` fp
    apply sub (TyArr t1 t2 fp)       = TyArr (sub `apply` t1) (sub `apply` t2) (sub `apply` fp)
    apply sub (TyApp t1 t2 fp)       = TyArr (sub `apply` t1) (sub `apply` t2) (sub `apply` fp)
    apply sub (TyVar var fp)           = TyVar var (sub `apply` fp)

    fv (TyCon _ fp)     = fv fp
    fv (TyArr t1 t2 fp) = fv t1 `Set.union` fv t2 `Set.union` fv fp
    fv (TyApp t1 t2 fp) = fv t1 `Set.union` fv t2 `Set.union` fv fp
    fv (TyVar _ fp)        = fv fp

instance Substitutable LVar FP TypeScm where
    apply s (TyForall as ty) = TyForall as $ apply s ty

    fv (TyForall as ty) = fv ty

instance Substitutable LVar FP FlppScm where
    apply (Subst s) (FpForall as cs ty) = FpForall as (apply sub' cs) (apply sub' ty)
        where sub' = Subst $ foldr Map.delete s as

    fv (FpForall as cs ty) = fv cs `Set.union` fv ty `Set.difference` Set.fromList as

instance Substitutable LVar FP FP where
    apply _ (FPSet lset)               = FPSet lset
    apply (Subst s) (FPVar var)        = Map.findWithDefault (FPVar var) var s

    fv (FPSet lset) = Set.empty
    fv (FPVar var)  = Set.singleton var

instance Substitutable LVar FP FConstraint where
    apply s (f1, f2) = (s `apply` f1, s `apply` f2)

    fv (f1, f2) = fv f1 `Set.union` fv f2

instance Substitutable LVar FP TConstraint where
    apply s (f1, f2) = (s `apply` f1, s `apply` f2)

    fv (f1, f2) = fv f1 `Set.union` fv f2

instance Substitutable LVar FP a => Substitutable LVar FP [a] where
    apply = map . apply

    fv = foldr (Set.union . fv) Set.empty

instance Substitutable LVar FP Env where
    apply s (Env e) = Env $ Map.map (apply s) e

    fv (Env e) = fv $ Map.elems e

instance Substitutable LVar FP a => Substitutable LVar FP (Decompose a) where
    apply s (ArrL x) = ArrL $ apply s x
    apply s (Is x) = Is $ apply s x

    fv (ArrL x) = fv x
    fv (Is x) = fv x

instance Substitutable LVar FP TypedProgram where
    apply s (loc, ty) = (loc, apply s ty) 

    fv (_, ty) = fv ty

instance Substitutable TVar Type TypedProgram where
    apply s (loc, ty) = (loc, apply s ty) 

    fv (_, ty) = fv ty
-- Solve type constraints

unify :: Type -> Type -> Solve (Subst TVar Type)
unify t1 t2 | t1 `typeEq` t2 = return emptySubst
unify (TyVar v _) t = v `bind` t
unify t (TyVar v _) = v `bind` t
unify (TyArr t1 t2 _) (TyArr t3 t4 _) = unifyMany [t1, t2] [t3, t4]
unify (TyApp t1 t2 _) (TyApp t3 t4 _) = unifyMany [t1, t2] [t3, t4]
unify t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve (Subst TVar Type)
unifyMany [] [] = return emptySubst
unifyMany (x:xs) (y:ys) = do
    s1 <- unify x y
    s2 <- unifyMany (s1 `apply` xs) (s1 `apply` ys)
    return $ s2 `compose` s1
unifyMany _ _ = throwError $ UnificationMismatch

runSolve :: [TConstraint] -> Either TypeError (Subst TVar Type)
runSolve cs = runTypeErrorM $ solver st
    where st = (emptySubst, cs)

solver :: (Subst TVar Type, [TConstraint]) -> Solve (Subst TVar Type)
solver (su, cs) = case cs of
    [] -> return su
    (t1,t2):xs -> do 
        s <- unify t1 t2
        solver (s `compose` su, apply s xs)

-- Solve flow properties constraints

type Relation = Map.Map LVar (Set.Set LVar, Set.Set Label)

runGen :: [FConstraint] -> Either TypeError Relation
runGen = runTypeErrorM . genMany

decompose :: Decompose Type -> TypeErrorM Type
decompose (Is a) = return a
decompose (ArrL a) = case a of
    TyArr t1 _ _ -> return t1
    _ -> throwError $ DecompositionFail (ArrL a)

(<.>) :: Relation -> Relation -> Relation
(<.>) = Map.unionWith (\(x1, y1) (x2, y2) -> (x1 `Set.union` x2, y1 `Set.union` y2))

emptyRelation = Map.empty

genMany :: [FConstraint] -> TypeErrorM Relation
genMany (x:xs) = do
    m1 <- gen x
    m2 <- genMany xs
    return $ m1 <.> m2
genMany [] = return $ emptyRelation

gen :: FConstraint -> TypeErrorM Relation
gen (c1, c2) = do
    c1' <- decompose c1
    c2' <- decompose c2
    case (c1', c2') of
        (TyCon t1 f1, TyCon t2 f2) | t1 == t2 -> subset f1 f2
        (TyArr t1 t2 f1, TyArr t3 t4 f2) -> do
            r1 <- genMany [(Is t3, Is t1), (Is t2, Is t4)] 
            r2 <- subset f1 f2
            return $ r1 <.> r2
        (TyApp t1 t2 f1, TyApp t3 t4 f2) -> do
            r1 <- genMany [(Is t1, Is t3), (Is t2, Is t4)]
            r2 <- subset f1 f2
            return $ r1 <.> r2
        (TyVar _ f1, x) -> subset f1 (ty2fp x)
        (x, TyVar _ f2) -> subset (ty2fp x) f2
        _ -> throwError $ GenerateFPConstraintFail c1 c2

subset :: FP -> FP -> TypeErrorM Relation
subset f1 f2 = case (f1, f2) of
    (FPVar v1, FPVar v2) -> 
        return $ Map.singleton v2 (Set.singleton v1, Set.empty)
    (FPSet s1, FPVar v2) -> 
        return $ Map.singleton v2 (Set.empty, s1)
    (FPSet s1, FPSet s2) | Set.isSubsetOf s1 s2 -> 
        return $ emptyRelation
    _ -> throwError $ IncompatibleFPConstraints f1 f2

extendSubst :: Subst LVar FP -> LVar -> FP -> Subst LVar FP
extendSubst (Subst s) x (FPSet l) = Subst $
    Map.insert x (FPSet $ l `Set.union` l') s
        where (FPSet l') = Map.findWithDefault (FPSet Set.empty) x s

update :: Relation -> LVar -> (Set.Set LVar, Subst LVar FP) -> (Set.Set LVar, Subst LVar FP)
update relation x (visit, sub) = 
    case x `Set.member` visit of
        True -> (visit, sub)
        False -> case Map.lookup x relation of
            Just (sons, fp) -> 
                let (visit', sub') = Set.foldr 
                        (update relation) 
                        (Set.insert x visit, extendSubst sub x (FPSet fp)) 
                        sons
                in (visit', Set.foldr (\y all@(Subst sub) -> 
                    case Map.lookup y sub of
                        Just lset -> extendSubst all x lset
                        Nothing -> all) sub' sons)
            Nothing -> (visit, sub)

accumulate :: Relation -> Subst LVar FP
accumulate relation = trace (groom $ Map.keys relation) $ foldr (\x y -> snd $ 
    update relation x (Set.empty, y))
    emptySubst $ Map.keys relation

