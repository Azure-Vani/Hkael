{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Pos as Pos
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

withPosParser :: Parser a -> Parser (a, SrcLoc)
withPosParser p = do
    start <- getPosition
    x <- p
    end <- getPosition
    return (x, SrcLoc start end)

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = do
  (x, srcLoc) <- withPosParser identifier
  return $ Var srcLoc x

number :: Parser Expr
number = do
  (n, srcLoc) <- withPosParser integer
  return $ Lit srcLoc (LInt (fromIntegral n))

bool :: Parser Expr
bool = (withPosParser (reserved "True") >>= 
            \(_, srcLoc) -> return $ Lit srcLoc (LBool True))
    <|> (withPosParser (reserved "False") >>=
            \(_, srcLoc) -> return $ Lit srcLoc (LBool False))

lambda :: Parser Expr
lambda = do
  start <- getPosition
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  end <- getPosition
  let srcLoc = SrcLoc start end
  return $ foldr (Lambda srcLoc) body args

letin :: Parser Expr
letin = do
  start <- getPosition
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  end <- getPosition
  return $ Let (SrcLoc start end) x e1 e2

ifthen :: Parser Expr
ifthen = do
  start <- getPosition
  reserved "if"
  cond <- aexp
  reservedOp "then"
  tr <- aexp
  reserved "else"
  fl <- aexp
  end <- getPosition
  return $ If (SrcLoc start end) cond tr fl

aexp :: Parser Expr
aexp =
      parens expr
  <|> bool
  <|> number
  <|> ifthen
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = Ex.buildExpressionParser table aexp

infixOp :: String -> (SrcLoc -> a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix $ 
    withPosParser (reservedOp x) >>= (\(_, srcLoc) -> return $ f srcLoc)

table :: Operators Expr
table = [
    [
      infixOp "*" (Op Mul) Ex.AssocLeft
    ],
    [
      infixOp "+" (Op Add) Ex.AssocLeft
    , infixOp "-" (Op Sub) Ex.AssocLeft
    ],
    [
      infixOp "==" (Op Eql) Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = do
  (es, srcLoc) <- withPosParser $ many1 term
  return (foldl1 (App srcLoc) es)

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "filename" input
