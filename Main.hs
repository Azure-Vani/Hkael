import Text.Groom
import Debug.Trace

import Control.Monad

import System.Environment

import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as L

import Text.Parsec.Pos

import Data
import Syntax
import Parser
import Infer

main = do
    filename <- liftM head $  getArgs
    program <- L.readFile filename
    case parseExpr program of
        Left err ->
            putStrLn $ "[Parse Error]: " ++ (groom err)
        Right expr -> 
            case Infer.inferExpr expr of
                Left err -> 
                    putStrLn $ "[Infer Error]: " ++ (groom err)
                Right (ty, tp) -> prettyPrintTypedProg tp

prettyPrintPos pos = do
    let (line, column) = (sourceLine pos, sourceColumn pos)
    putStr "("
    putStr $ show line
    putStr ","
    putStr $ show column
    putStr ")"

prettyPrintSrcLoc (SrcLoc start end) = do
   putStr "["
   prettyPrintPos start
   putStr ","
   prettyPrintPos end
   putStr "]"

prettyPrintTypedProg :: [TypedProgram] -> IO ()
prettyPrintTypedProg [] = return ()
prettyPrintTypedProg (x:xs) = do
    let (srcLoc, ty) = x
    prettyPrintSrcLoc srcLoc
    putStr " -> "
    let (FPSet fpSet) = ty2fp ty 
    mapM_ (\(Label srcLoc) -> prettyPrintSrcLoc srcLoc >> putStr ", ") (Set.toList fpSet)
    putStrLn ""
    prettyPrintTypedProg xs
