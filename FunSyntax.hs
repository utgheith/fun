{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module FunSyntax(parse, prog, term, Term(BinaryOp, Block, Call, Const, FunDef, Negate, VarRef)) where

import Control.Monad.State.Lazy (runStateT)

-- import Debug.Trace (trace)

import FunLexer (lexer, Token(Ident, Num, Keyword, Symbol))
import ParserCombinators (oneof, Parser, Result, rpt, rptDropSep, satisfy, token)
import Data.Set qualified as S

data Term =
    BinaryOp String Term Term
    | Block [Term]
    | Call Term [Term]
    | Const Integer
    | FunDef String [String] Term
    | Negate Term
    | VarRef String
    -- | more term constructors
    deriving (Show, Eq)

-- succeed if the next token is the given symbol
symbol :: String -> Parser Token ()
-- using explicit bind
symbol s = token (Symbol s) >>= \_ -> return ()

-- succeed if the next token is the given keyword
keyword :: String -> Parser Token ()
-- using do notation (syntactic sugar for >>=)
keyword k = do
    _ <- token $ Keyword k
    return ()

-- identifier
ident :: Parser Token String
ident = satisfy $ \case
    Ident name -> Just name
    _ -> Nothing

-- symbol
checkSymbol :: (String -> Bool) -> Parser Token String
checkSymbol predicate = satisfy $ \case
    Symbol s | predicate s -> Just s
    _ -> Nothing

----------
-- term --
----------

term :: Parser Token Term
term = binaryExp precedence


------------------- binary operators (left associative) -------------------

-- precedence levels, from lowest to highest
precedence :: [S.Set String]
precedence = [S.fromList ["+"], S.fromList ["*", "/"]]

binaryExp :: [S.Set String] -> Parser Token Term
binaryExp [] = unaryExp
binaryExp (ops:rest) = do
    -- lhs
    lhs <- binaryExp rest

    -- find the longest sequence of (op, subexpression) at this precedence level
    -- then combine them left to right
    rhss <- rpt $ do
        op <- checkSymbol (`S.member` ops)
        rhs <- term
        return (op, rhs)

    -- combine results left to right
    return $ foldl (\acc (op, rhs) -> BinaryOp op acc rhs) lhs rhss

------------------- unary operators  -------------------

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [ Negate e | _ <- symbol "-", e <- unaryExp ]

num :: Parser Token Term
num = do
    n <- satisfy $ \case
        Num n -> Just n
        _ -> Nothing
    return $ Const n

parans :: Parser Token Term
parans = [t | _ <- symbol "(", t <- term, _ <- symbol ")"]

funDef :: Parser Token Term
funDef = [ FunDef name params body | _ <- keyword "fun",
    name <- ident,
    _ <- keyword "(",
    params <- rptDropSep ident (symbol ","),
    _ <- symbol ")",
    body <- term
    ]

varRef :: Parser Token Term
varRef = VarRef <$> ident

block :: Parser Token Term
block = do
    _ <- token $ Symbol "{"
    ts <- rpt term
    _ <- token $ Symbol "}"
    return $ Block ts

unaryExp :: Parser Token Term
unaryExp = oneof [block, funDef, minus, num, parans, varRef]

----------- prog ----------

prog :: Parser Token Term
prog = Block <$> rpt term

----------- parse ----------

parse :: [Char] -> Parser Token a -> Result (a, [Token])
parse input p = let
    tokens = lexer input in
    runStateT p tokens

