import Text.Groom
import Debug.Trace

import Syntax 
import Infer

expr1 = App 1 (Lambda 2 "x" 
                (App 3 
                   (Var 4 "x") 
                   (Lit 5 (LBool True))))
             (If 6 (Lit 7 (LBool True)) 
                   (Lambda 8 "y" (Var 9 "y")) 
                   (Lambda 10 "z" (Var 11 "z")))

expr2 = Let 1 "id" (Lambda 2 "x" 
                        (If 3 (Var 4 "x") (Var 5 "x") (Var 6 "x")))
                   (App 7 (Var 8 "id") (Lit 9 (LBool True)))

main = case Infer.inferExpr expr2 of
    Left err -> 
        putStrLn $ "[Error]: " ++ (groom err)
    Right (ty, tp) -> do 
        putStrLn $ "Type: " ++ (groom ty)
        putStrLn $ "TypedProgram: " ++ (groom tp)

