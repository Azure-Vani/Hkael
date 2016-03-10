module LambdaCore.Utils where

import LambdaCore.Data
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

newtype FreshCounter = FreshCounter { getFreshCounter :: Int }

initFreshCounter :: FreshCounter
initFreshCounter = initFreshCounterAt 0

initFreshCounterAt :: Int -> FreshCounter
initFreshCounterAt i = FreshCounter { getFreshCounter = i }

runWithFreshCounter :: ExceptT e (State FreshCounter) a -> Either e a
runWithFreshCounter e = evalState (runExceptT e) initFreshCounter

toLisp :: Exp -> String
toLisp (Lit (IntV v)) = show v
toLisp (Lit (BoolV v)) = if v then "true" else "false"
toLisp (Lit (StringV v)) = show v
toLisp (Var v) = v
toLisp (Fun vs body) = "(fn (" ++ unwords (map toLisp vs) ++ ") " ++ toLisp body ++ ")"
toLisp (App vs body) = "(" ++ toLisp vs ++ " " ++ toLisp body ++ ")"
toLisp (Let v e body) = "(let (" ++ toLisp v ++ " " ++ toLisp e ++ ") " ++ toLisp body ++ ")"
toLisp (If p t e) = "(if " ++ toLisp p ++ " " ++ toLisp t ++ " " ++ toLisp e ++ ")"
toLisp (BinOp op a b) = "(" ++ op ++ " " ++ toLisp a ++ " " ++ toLisp b ++ ")"

