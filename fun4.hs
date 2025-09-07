{-# LANGUAGE MonadComprehensions #-}

import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as SM
import Control.Monad (join, when)

-- Key point: better error handling

-------- Environment ---------

data Env = Env { symbolTable :: M.Map String Integer    -- maps variable names
                                                        -- to values
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

----------------------------------------------------------
-- A term denotes a stateful computations that may fail --
----------------------------------------------------------

-- We need to define how 'fail' works for Either
instance MonadFail (Either String) where
    fail :: String -> Either String a
    fail = Left

-- Monad transformer stacking State on top of Either.
--   Now we don't have to handle errors manually. We just use 'fail' and
--   the computation stops with an error message.
eval :: Term -> SM.StateT Env (Either String) Integer

eval (Literal n) =
    return n

eval (Add e1 e2) =
    [ v1 + v2 | v1 <- eval e1, v2 <- eval e2]

eval Read  = do
    env <- SM.get
    case input env of
        [] ->
            fail $ "Input exhausted" ++ " [state: " ++ show env ++ "]"
        (i:is) -> do
            _ <- SM.put env { input = is }
            return i

eval (Var name) = do
    env <- SM.get
    case M.lookup name (symbolTable env) of
        Nothing -> fail $ "Undefined variable: " ++ name ++ " [state: " ++ show env ++ "]"
        Just val -> return val

eval (Assignment name rhs) = do
    v <- eval rhs
    _ <- SM.modify (\env -> env { symbolTable = M.insert name v (symbolTable env) })
    return v

eval (If cond thenStmt elseStmt)  = do
    v <- eval cond
    if v /= 0 then eval thenStmt  else eval elseStmt

eval (While cond body) = loop 0 where
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

run :: Program -> [Integer] -> Either String (Integer, Env)
run (Program stmt) stdin =
    let env = Env { symbolTable = M.empty, input = stdin, output = [] }
    in SM.runStateT (eval stmt) env

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
main = do
    print $ run exampleProgram [42]
    print $ run exampleProgram [] -- should fail due to Read on empty input
