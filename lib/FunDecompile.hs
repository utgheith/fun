
module FunDecompile(decompile) where

import FunSyntax (Term(Assign, BinaryOp, Block, Call, Const, IfThenElse, FunDef, Negate, VarDef, VarRef, While))
import Data.List (intercalate)

decompile :: Term -> String

decompile (Assign name expr) = name ++ " = " ++ decompile expr

decompile (BinaryOp op lhs rhs) = "(" ++ decompile lhs ++ " " ++ op ++ " " ++ decompile rhs ++ ")"

decompile (Block terms) = "{ " ++ intercalate "\n" (map decompile terms) ++ " }"

decompile (Call name args) = decompile name ++ "(" ++ intercalate ", " (map decompile args) ++ ")"

decompile (Const n) = show n

decompile (FunDef name params body) = "fun " ++ name ++ "(" ++ intercalate ", " params ++ ") = " ++ decompile body

decompile (IfThenElse cond thenTerm elseTerm) =
    "if " ++ decompile cond ++ " " ++ decompile thenTerm ++ maybe "" (\et -> " else " ++ decompile et) elseTerm

decompile (Negate t) = "-" ++ decompile t

decompile (VarDef name expr) = "var " ++ name ++ maybe "" (\i -> " = " ++ decompile i) expr

decompile (VarRef name) = name

decompile (While cond body) = "while " ++ decompile cond ++ " " ++ decompile body

            
