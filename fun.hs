
import FunSyntax (parse, Term(BinaryOp, Block, Call, Const, FunDef, Negate, VarRef), prog)
import Data.List (intercalate)


decompile :: Term -> String
decompile (BinaryOp op lhs rhs) = "(" ++ decompile lhs ++ " " ++ op ++ " " ++ decompile rhs ++ ")"
decompile (Block terms) = "{ " ++ intercalate "\n" (map decompile terms) ++ " }"
decompile (Call name args) = decompile name ++ "(" ++ intercalate ", " (map decompile args) ++ ")"
decompile (Const n) = show n
decompile (FunDef name params body) = "fun " ++ name ++ "(" ++ intercalate ", " params ++ ") = " ++ decompile body
decompile (Negate t) = "-" ++ decompile t
decompile (VarRef name) = name

main :: IO ()
main = do
    text <- getContents
    let r = parse text prog
    case r of
        Left err -> print err
        Right (term, rest) -> do
            putStrLn $ decompile term
            print rest
            
