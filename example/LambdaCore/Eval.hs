module LambdaCore.Eval where

import LambdaCore.Data
import LambdaCore.Utils
import LambdaCore.TypeChecker
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type EvalEnv = M.Map VarName Exp

type Answer = (Exp, Type)

data EvalError = UnboundedVariable Exp VarName
               | InvalidExpression Exp
               | GenericError Exp String

type ProgramError = Either TypeError EvalError

programError :: ProgramError -> String
programError pe = case pe of
  Left typeError -> error $ show typeError
  Right evalError -> error $ show evalError

-- | Substitute variables in expression, but don't apply.
--
subst :: EvalEnv -> Exp -> Exp
subst env expr = case expr of
  Lit {} -> expr
  Var n -> fromMaybe (Var n) (M.lookup n env)
  Fun vs body -> Fun vs (subst env body)
  BinOp f a b -> BinOp f (subst env a) (subst env b)
  e -> e

defaultEnv :: M.Map VarName Exp
defaultEnv = M.fromList [
  ("+", Fun [Var "a",Var "b"] (BinOp "+" (Var "a") (Var "b")))
 ,("-", Fun [Var "a",Var "b"] (BinOp "-" (Var "a") (Var "b")))
 ,("*", Fun [Var "a",Var "b"] (BinOp "*" (Var "a") (Var "b")))

 ,("and", Fun [Var "a",Var "b"] (BinOp "and" (Var "a") (Var "b")))
 ,("or", Fun [Var "a",Var "b"] (BinOp "or" (Var "a") (Var "b")))
 ,("xor", Fun [Var "a",Var "b"] (BinOp "xor" (Var "a") (Var "b")))

 ,("++", Fun [Var "a",Var "b"] (BinOp "++" (Var "a") (Var "b")))
 ]

evalExp :: Exp -> Answer
evalExp expr =
  case eval expr of
    Left err -> case err of
      Left typeError -> error $ show typeError
      Right evalError -> error $ show evalError
    Right answer -> answer

-- | Evaluates expression.
--
eval :: Exp -> Either ProgramError Answer
eval expr =
  case runWithFreshCounter (checkType expr) of
    Left typeError -> Left (Left typeError)
    Right t ->
     case eval' defaultEnv expr of
       Left evalError -> Left (Right evalError)
       Right expr' -> return (expr', t)

eval' :: EvalEnv -> Exp -> Either EvalError Exp
eval' env expr = case expr of

  Lit{} -> return expr

  Var n -> case M.lookup n env of
      Nothing -> Left (UnboundedVariable expr n)
      Just x -> return x

  Fun vs e -> return $ Fun vs (subst env e)

  -- Two cases for function application:
  --
  -- - If `f` is a function, apply the expression into the function.
  -- - Otherwise, re-try the application after eval-ing `f`.
  --
  App f e -> do
    e' <- eval' env e
    case f of
      -- Only one arg left, so do the apply.
      Fun [Var n] fn -> do
        let env' = M.insert n e' env
        eval' env' fn

      -- Curry; apply only one level.
      Fun (Var n:vs) fn -> do
        let env' = M.insert n e' env
        eval' env' (Fun vs fn)

      other -> do
        f' <- eval' env other
        eval' env (App f' e)

  Let (Var n) val body -> do
    val' <- eval' env val
    let env' = M.insert n val' env
    eval' env' body
  Let{} -> Left $ InvalidExpression expr

  If p t e -> do
    p' <- eval' env p
    case p' of
      Lit (BoolV True) -> eval' env t
      Lit (BoolV False) -> eval' env e
      _ -> Left $ GenericError expr "If statement accept only boolean predicates."

  BinOp fn a b -> case fn of
    "+" -> do
      a' <- eval' env a >>= extractInt
      b' <- eval' env b >>= extractInt
      return $ Lit (IntV (a' + b'))
    "-" -> do
      a' <- eval' env a >>= extractInt
      b' <- eval' env b >>= extractInt
      return $ Lit (IntV (a' - b'))
    "*" -> do
      a' <- eval' env a >>= extractInt
      b' <- eval' env b >>= extractInt
      return $ Lit (IntV (a' * b'))

    "and" -> do
      a' <- eval' env a >>= extractBool
      b' <- eval' env b >>= extractBool
      return $ Lit (BoolV (a' && b'))
    "or" -> do
      a' <- eval' env a >>= extractBool
      b' <- eval' env b >>= extractBool
      return $ Lit (BoolV (a' || b'))
    "xor" -> do
      a' <- eval' env a >>= extractBool
      b' <- eval' env b >>= extractBool
      return $ Lit (BoolV ((a' && not b') || (not a' && b')))

    "++" -> do
      a' <- eval' env a >>= extractString
      b' <- eval' env b >>= extractString
      return $ Lit (StringV (a' ++ b'))

    _ -> Left $ GenericError expr "Not a valid binary operation."

extractInt :: Exp -> Either EvalError Int
extractInt (Lit (IntV v)) = return v
extractInt e = Left $ GenericError e "Not an Int."

extractBool :: Exp -> Either EvalError Bool
extractBool (Lit (BoolV v)) = return v
extractBool e = Left $ GenericError e "Not a Bool."

extractString :: Exp -> Either EvalError String
extractString (Lit (StringV v)) = return v
extractString e = Left $ GenericError e "Not a String."

instance Show EvalError where
  show (UnboundedVariable e vn) = "Eval error: " ++ show vn ++ " is not bound in expression " ++ toLisp e
  show (InvalidExpression e) = "Eval error:" ++ toLisp e ++ " is an invalid expression"
  show (GenericError e msg) = "Eval error:" ++ show msg ++ " in expression " ++ toLisp e
