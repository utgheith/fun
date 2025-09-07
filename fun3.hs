{-# LANGUAGE MonadComprehensions #-}

import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as SM
import Control.Monad (join, when)


-- Key point: why have the artificial distinction between expressions and statements?


-------- Environment ---------

data Env = Env { symbolTable :: M.Map String Integer -- maps variable names to values
               , input :: [Integer] -- input stream (used by read)
               , output :: [Integer] -- output stream (used by write)
               } deriving Show

-------- Terms ---------

data Term = Var String
            | Literal Integer           -- returns its value
            | Add Term Term             -- returns the sum of its arguments
            | Read                      -- reads the next integer from the input stream
            | Assignment String Term    -- returns the value assigned to the variable
            | If Term Term Term         -- returns the value of the selected branch
            | While Term Term           -- returns the value of the last body execution or 0
            | Sequence [Term]           -- returns the value of the last statement or 0
            | Write Term                -- returns the value written
            deriving Show

eval :: Term -> SM.State Env Integer

eval (Literal n) =
    return n

eval (Add e1 e2) =
    [v1 + v2 | v1 <- eval e1, v2 <- eval e2]

eval Read  = do
    env <- SM.get
    case input env of
        [] ->
            error "Input exhausted"
        (i:is) -> do
            _ <- SM.put env { input = is }
            return i

-- Point-free style is hard to read at first but leads to concise code
eval (Var name) = 
        M.findWithDefault (error $ "Undefined variable: " ++ name) name . symbolTable <$> SM.get

eval (Assignment name rhs) = do
    v <- eval rhs
    _ <- SM.modify (\env -> env { symbolTable = M.insert name v (symbolTable env) })
    return v

eval (If cond thenStmt elseStmt)  = do
    v <- eval cond
    if v /= 0 then eval thenStmt  else eval elseStmt

eval s@(While cond body) = loop 0 where
    loop last = do
        v <- eval cond
        if v == 0 then
            return last
        else do
            x <- eval body
            loop x


eval (Sequence ss) = loop 0 ss where
    loop last [] = return last
    loop _ (s:ss) = do
        x <- eval s
        loop x ss

eval (Write e)  = do
    v <- eval e
    _ <- SM.modify $ \env -> env { output = output env ++ [v] }
    return v


-------- Programs ---------

newtype Program = Program Term

run :: Program -> [Integer] -> (Integer, Env)
run (Program stmt) stdin =
    let env = Env { symbolTable = M.empty, input = stdin, output = [] }
    in SM.runState (eval stmt) env

-------- Example Program ---------

exampleProgram :: Program
exampleProgram = Program (Sequence
    [ Write $ Assignment "x" (Literal 10)
    , Write $ Assignment "y" (Literal 20)
    , Write $ If (Var "x")
        (Write (Var "y"))
        (Write (Literal 0))
    , Write $ While (Var "x")
        (Sequence
            [ Write (Var "x")
            , Assignment "x" (Add (Var "x") (Literal (-1)))
            ])
    , Write $ Write Read
    ])


main :: IO ()
main = print $ run exampleProgram [42]
