module LambdaCore.Data where

import qualified Data.Set as S

-- Program code
type Program = String

-- Variable name
type VarName = String

-- Type variable
type TName = String

reservedIds :: S.Set String
reservedIds = S.fromList ["fn", "let"]

-- | Symbols that can be part of symbol-only identifiers.
validIdSymbols :: String
validIdSymbols = "<>=%^*-+/"

data Value = IntV Int
           | BoolV Bool
           | StringV String
  deriving (Show, Eq)

data Exp = Lit Value
         | Var VarName       -- ^ Var "x"
         | Fun [Exp] Exp
         | App Exp Exp       -- ^ App (Fun or Var) (Argument value)
         | Let Exp Exp Exp   -- ^ Let (Var "x") (Value of x) Body.
         | If Exp Exp Exp    -- ^ If (Predicate : Bool) (Then clause) (Else clause)
         | BinOp String Exp Exp
  deriving (Show, Eq)

data Type = TUnit -- TODO: Temporary
          | TInt
          | TBool
          | TString
          | TFun [Type]
          | TVar TName
  deriving (Eq, Ord)

data TypeError = Mismatch Type Type
               | FunctionExpected Type
               | UnboundVariable VarName
               | InfiniteType Type Type -- InfiniteType TVar Type
               | GenericTypeError (Maybe String)
  deriving (Eq)

instance Show TypeError where
  show (Mismatch t1 t2) = "type mismatch: expecting " ++ show t1 ++ " but got " ++ show t2
  show (FunctionExpected t) = "type mismatch: expecting function but got " ++ show t
  show (UnboundVariable n) = "unbound variable " ++ n
  show (InfiniteType tvar t) = "cannot resolve infinite type " ++ show tvar ++ " in " ++ show t
  show (GenericTypeError (Just msg)) = "type error: " ++ msg
  show (GenericTypeError Nothing) = "type error"

instance Show Type where
  show TUnit = "Unit"
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "String"
  show (TFun ts) = "(-> " ++ unwords (map show ts) ++ ")"
  show (TVar n) = n

