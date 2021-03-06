module Data where

import qualified Data.Set as Set

import Syntax

-- Flow Properties
newtype Label = Label SrcLoc
    deriving (Show, Eq, Ord)

newtype LVar = LVar String
    deriving (Show, Eq, Ord)

-- flow properties constraint
type FConstraint = (Type, Type)

data FP = FPSet (Set.Set Label)
        | FPVar LVar 
    deriving (Show, Eq, Ord)

-- Types
newtype TVar = TVar String 
    deriving (Show, Eq, Ord)

type TConstraint = (Type, Type)

data Type = TyCon String FP    -- type constructors
          | TyArr Type Type FP -- function type
          | TyApp Type Type FP -- application of a type constructor
          | TyVar TVar FP      -- a type variable
    deriving (Show, Eq, Ord)

data FlppScm = FpForall [LVar] [FConstraint] Type
    deriving (Show, Eq, Ord)
data TypeScm = TyForall [TVar] FlppScm
    deriving (Show, Eq, Ord)

type TypedProgram = (SrcLoc, Type)

data TypeError 
    = InfinitType String String
    | UnificationFail Type Type
    | UnboundVariables Name
    | UnificationMismatch
    | IncompatibleFPConstraints FP FP
    | GenerateFPConstraintFail Type Type
    | DecompositionFail Type
    deriving (Show, Eq, Ord)

typeEq :: Type -> Type -> Bool
typeEq (TyCon s1 _) (TyCon s2 _) | s1 == s2 = True
typeEq (TyArr t1 t2 _) (TyArr t3 t4 _) 
    | t1 `typeEq` t3 && t2 `typeEq` t4      = True
typeEq (TyApp t1 t2 _) (TyApp t3 t4 _) 
    | t1 `typeEq` t3 && t2 `typeEq` t4      = True
typeEq (TyVar v1 _) (TyVar v2 _) | v1 == v2     = True
typeEq _ _ = False

modifyFp :: Type -> FP -> Type
modifyFp (TyCon x _) fp = TyCon x fp
modifyFp (TyArr x y _) fp = TyArr x y fp
modifyFp (TyApp x y _) fp = TyApp x y fp
modifyFp (TyVar x _) fp = TyVar x fp

ty2fp :: Type -> FP
ty2fp (TyCon _ fp) = fp
ty2fp (TyArr _ _ fp) = fp
ty2fp (TyApp _ _ fp) = fp
ty2fp (TyVar _ fp) = fp

typeInt, typeBool, typeChar :: FP -> Type
typeInt  = TyCon "Int"
typeBool = TyCon "Bool"
typeChar = TyCon "Char"
typeDummy = TyCon "Dummy"

fpSingleton loc = FPSet $ Set.singleton $ Label loc
