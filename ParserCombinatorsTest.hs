import Test.HUnit
import ParserCombinators

tests = TestList [
    TestCase (assertEqual "test1" (parse "1 + 2" expr) (Right (BinaryOp "+" (Const 1) (Const 2), []))),
    TestCase (assertEqual "test2" (parse "fun x() = 42" expr) (Right (FunDef "x" [] (Const 42), [])))
]