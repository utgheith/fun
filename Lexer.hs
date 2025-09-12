module Lexer(lexer, Token(Num, Ident, Keyword, Symbol)) where

import Data.List (unfoldr)
import Data.Char (isNumber, isSpace, isSymbol, isAlpha, isAlphaNum)
import Data.Set (fromList, member)

data Token = Num Integer
        | Ident String
        | Keyword String
        | Symbol String
        | Error String
        deriving (Show, Eq)

other = fromList "(){}[],"

keywords = fromList [
    "fun", "var", "if", "else", "while", "print", "try", "catch"]


lexer :: [Char] -> [Token]
lexer  = unfoldr step where
    step [] = Nothing
    -- skip spaces and new lines
    step (c : rest) | isSpace c = step rest
    -- numbers
    step s@(c : _) | isNumber c =
        let (num, rest) = span isNumber s
        in Just (Num $ read num, rest)
    -- identifiers and keywords
    step s@(c : _) | isAlpha c =
        let (var, rest) = span isAlphaNum s
        in Just (if member var keywords then Keyword var else Ident var, rest)
    -- symbols
    step s@(c : _) | isSymbol c =
        let (var, rest) = span isSymbol s
        in Just (Symbol var, rest)
    -- other operators
    step s@(c: _) | member c other =
        let (var, rest) = span (`member` other) s
        in Just (Symbol var, rest)

    step s = Just (Error ("Unexpected character: " ++ take 20 s), "")