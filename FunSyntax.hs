{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module FunSyntax(parse, Term(BinaryOp, Block, Call, Const, FunDef, Negate, VarRef), prog) where

import Control.Monad.State.Lazy (runStateT)

import Lexer (lexer, Token(Ident, Num, Keyword, Symbol))
import Parser (oneof, opt, Parser, Result, rpt, rptsep, satisfy, token)
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

-----------------
-- expressions --
-----------------

expr :: Parser Token Term
expr = exp2 binaryGroups


binaryGroups :: [S.Set String]
binaryGroups = S.fromList <$> [["-", "+"], ["*"]]

exp2 :: [S.Set String] -> Parser Token Term
exp2 = \case
    [] -> exp1
    ops:rest -> do
        lhs <- exp2 rest
        rhs <- opt $ do
            op <- checkSymbol (`S.member` ops)
            rhs <- expr
            return (op, rhs)
        return $ case rhs of
            Just (op, r) -> BinaryOp op lhs r
            Nothing -> lhs

minus :: Parser Token Term
minus = do
    _ <-token $ Symbol "-"
    Negate <$> expr

num :: Parser Token Term
num = do
    n <- satisfy $ \case
        Num n -> Just n
        _ -> Nothing
    return $ Const n

parans :: Parser Token Term
parans = do
    _ <- token $ Symbol "("
    t <- expr
    _ <-token $ Symbol ")"
    return t

funDef :: Parser Token Term
funDef = do
    _ <- token $ Keyword "fun"
    name <- ident
    _ <-token $ Symbol "("
    params <- rptsep ident (token $ Symbol ",")
    _ <- token $ Symbol ")"
    body <- term
    return $ FunDef name params body

varRef :: Parser Token Term
varRef = VarRef <$> ident


exp1 :: Parser Token Term
exp1 = oneof [funDef, minus, num, parans, varRef]

fun :: Parser Token Term
fun = do
    _ <- token $ Keyword "fun"
    name <- ident
    _ <- token $ Symbol "("
    params <- rptsep ident (token $ Symbol ",")
    _ <- token $ Symbol ")"
    body <- term 
    return $ FunDef name params body

block :: Parser Token Term
block = do
    _ <- token $ Symbol "{"
    ts <- rpt term
    _ <- token $ Symbol "}"
    return $ Block ts

----------- term ----------

term :: Parser Token Term
term = oneof [block, fun, expr]


terms :: Parser Token Term
terms = do
    ts <- rpt term
    return $ Block ts

prog :: Parser Token Term
prog = terms


----------- parse ----------

parse :: [Char] -> Parser Token a -> Parser.Result (a, [Token])
parse input p = let
    tokens = lexer input in
    runStateT p tokens

