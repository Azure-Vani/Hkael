module Data where

import Language.Haskell.Exts (SrcLoc)

-- Type variable in let polymorphic
type TName = Int

-- Label variable
type LName = Int

-- Variables
type VName = String

-- Assign every term an unique label
type Label = Int

-- A set of labels for annotated type
type LabelSet = [Label]

-- Used to track term type
type Id = Int

-- Flow Properties
data Fp = Lset LabelSet
        | Variable LName
        | Union Fp Fp
        | DummyFp

-- Ordinary type for ML type system
data Type = TyTuple [Type] Fp
          | TyList Type Fp
          | TyFun Type Type Fp
          | TyApp Type Type Fp
          | TyVar TName Fp
          | TyCon String Fp
          | TyForall TName Type Fp
          | LForall LName FpCons Type

type TyEnv     = [(VName, Type)]
type FpCons    = [(Fp, Fp)]
type TyCons    = [(Type, Type)]
type TypedProg = [(Id, Type)]

tyChar   = "char"
tyString = "string"
tyInt    = "int"
tyBool   = "bool"

atomfp l = Lset [l]
