
module FunDecompile(decompile) where

import FunSyntax (Term(BinaryOp, Block, Call, Const, FunDef, Negate, VarRef))
import Data.List (intercalate)

decompile :: Term -> String

decompile (BinaryOp op lhs rhs) = "(" ++ decompile lhs ++ " " ++ op ++ " " ++ decompile rhs ++ ")"

decompile (Block terms) = "{ " ++ intercalate "\n" (map decompile terms) ++ " }"

decompile (Call name args) = decompile name ++ "(" ++ intercalate ", " (map decompile args) ++ ")"

decompile (Const n) = show n

decompile (FunDef name params body) = "fun " ++ name ++ "(" ++ intercalate ", " params ++ ") = " ++ decompile body

decompile (Negate t) = "-" ++ decompile t

decompile (VarRef name) = name

            
