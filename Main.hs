import Syntax 
import Infer

expr = Lambda 1 "x" (Op 2 Add (Var 3 "x") (Lit 4 (LInt 1)))

main = case Infer.inferExpr expr of
    Left err -> 
        putStrLn $ "[Error]: " ++ (show err)
    Right (ty, tp) -> do 
        putStrLn $ "Type: " ++ (show ty)
        putStrLn $ "TypedProgram: " ++ (show tp)
