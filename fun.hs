import qualified Data.Map as M

-------- Environment ---------

data Env = Env { symbolTable :: M.Map String Integer -- maps variable names to values
               , input :: [Integer] -- input stream (used by read)
               , output :: [Integer] -- output stream (used by write)
               } deriving Show

-------- Expressions, return an interger value, have side effect ---------

data Expression = Var String
                | Literal Integer
                | Add Expression Expression
                | Read

evalExp :: Expression -> Env -> (Env, Integer)

evalExp (Literal n) env =
    (env, n)

evalExp (Add e1 e2) env = 
    let (env1, v1) = evalExp e1 env
        (env2, v2) = evalExp e2 env1
    in (env2, v1 + v2)

evalExp Read env =
    case input env of
        [] -> error "No more input"
        (i:is) -> (env {input = is}, i)

evalExp (Var x) env = 
    case M.lookup x (symbolTable env) of
        Just v -> (env, v)
        Nothing -> error ("Variable " ++ x ++ " not found")


-------- Statements, have side effects ----------

data Statement = Assignment String Expression
               | If Expression Statement Statement
               | While Expression Statement
               | Sequence [Statement]
               | Write Expression

evalStatement :: Statement -> Env -> Env
evalStatement (Assignment x rhs) env =
    let (env', v) = evalExp rhs env
    in env' { symbolTable = M.insert x v (symbolTable env') }
evalStatement (If cond thenStmt elseStmt) env =
    let (env', v) = evalExp cond env
    in if v /= 0 then evalStatement thenStmt env' else evalStatement elseStmt env'
evalStatement (While cond body) env =
    let (env', v) = evalExp cond env
    in if v /= 0 then evalStatement (While cond body) (evalStatement body env') else env'
evalStatement (Sequence []) env =
    env
evalStatement (Sequence (s:ss)) env =
    let env' = evalStatement s env
    in evalStatement (Sequence ss) env'
evalStatement (Write e) env =
    let (env', v) = evalExp e env
    in env' { output = output env' ++ [v] }


-------- Programs ---------

newtype Program = Program Statement

eval :: Program -> [Integer] -> Env
eval (Program stmt) stdin =
    let env = Env { symbolTable = M.empty, input = stdin, output = [] }
    in evalStatement stmt env

-------- Example Program ---------

exampleProgram :: Program
exampleProgram = Program (Sequence
    [ Assignment "x" (Literal 10)
    , Assignment "y" (Literal 20)
    , If (Var "x")
        (Write (Var "y"))
        (Write (Literal 0))
    , While (Var "x")
        (Sequence
            [ Write (Var "x")
            , Assignment "x" (Add (Var "x") (Literal (-1)))
            ])
    , Write Read
    ])



main :: IO ()
main = print $ eval exampleProgram [42]



