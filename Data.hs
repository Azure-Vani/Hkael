module Hkael.Data where

-- Type varibale in let polymorphic
type TName = Int

-- Assign every term an unique label
type Label = Int

-- A set of labels for annotated type
type LSet = [Label]

-- Ordinary type for ML type system
data Type = TUnit 
          | TInt
          | TBool
          | TString
          | TFun [Type]
          | TVar TName

-- Annotated type for control flow analysis
type AType = (Type, LSet)
