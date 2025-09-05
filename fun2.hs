import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as SM
import Control.Monad (when)


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

evalExp :: Expression -> SM.State Env Integer

evalExp (Literal n) =
    return n
    

evalExp (Add e1 e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    return (v1+v2)
        
evalExp Read  = do
    env <- SM.get
    case input env of
        [] ->
            error "No more input"
        (i:is) -> do
            SM.put env { input = is }
            return i

evalExp (Var name)  = do
    env <- SM.get 
    return $ M.findWithDefault (error ("Variable " ++ name ++ " not found")) name (symbolTable env)



-------- Statements, have side effects ----------

data Statement = Assignment String Expression
               | If Expression Statement Statement
               | While Expression Statement
               | Sequence [Statement]
               | Write Expression

evalStatement :: Statement -> SM.State Env ()
evalStatement (Assignment name rhs) = do
    v <- evalExp rhs
    SM.modify (\env -> env { symbolTable = M.insert name v (symbolTable env) })
    
evalStatement (If cond thenStmt elseStmt)  = do
    v <- evalExp cond
    if v /= 0 then evalStatement thenStmt  else evalStatement elseStmt

evalStatement s@(While cond body)  = do
    v <- evalExp cond
    when (v /= 0) $ do
        _ <- evalStatement body
        evalStatement s

evalStatement (Sequence []) =
    return ()
evalStatement (Sequence (s:ss))  = do
    _ <- evalStatement s
    evalStatement (Sequence ss)

evalStatement (Write e)  = do
    v <- evalExp e
    SM.modify (\env -> env { output = output env ++ [v] })


-------- Programs ---------

newtype Program = Program Statement

eval :: Program -> [Integer] -> Env
eval (Program stmt) stdin =
    let env = Env { symbolTable = M.empty, input = stdin, output = [] }
    in SM.execState (evalStatement stmt) env

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