module Syntax where

type Name = String

type SrcLoc = Int

data Expr
    = Lit SrcLoc Lit
    | Var SrcLoc Name
    | App SrcLoc Expr Expr
    | Lambda SrcLoc Name Expr
    | Let SrcLoc Name Expr Expr
    | If SrcLoc Expr Expr Expr
    | Fix SrcLoc Name Expr
    | Op SrcLoc Binop Expr Expr
    deriving (Show, Eq, Ord)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql deriving (Show, Eq, Ord)

type Decl = (String, Expr)

data Program = Program [Decl] Expr deriving (Show, Eq, Ord)

