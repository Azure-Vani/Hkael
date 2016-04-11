import Text.Groom

import Syntax 
import Infer

expr = App 1 (Lambda 2 "x" 
                (App 3 
                   (Var 4 "x") 
                   (Lit 5 (LBool True))))
             (If 6 (Lit 7 (LBool True)) 
                   (Lambda 8 "y" (Var 9 "y")) 
                   (Lambda 10 "z" (Var 11 "z")))

main = case Infer.inferExpr expr of
    Left err -> 
        putStrLn $ "[Error]: " ++ (groom err)
    Right (ty, tp) -> do 
        putStrLn $ "Type: " ++ (groom ty)
        putStrLn $ "TypedProgram: " ++ (groom tp)

