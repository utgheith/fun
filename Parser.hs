module Parser where

import Lexer (lexer, Token(Ident, Num, Keyword, Symbol))
import GHC.Conc (par)

data Term =
    Call Term [Term] 
    -- | more term constructors
    deriving (Show, Eq)


missing s = error ("Missing case in parser: " ++ s)


-----------------
-- parseParams --
-----------------

extractParams :: [Token] -> ([String], [Token])
extractParams (Ident param : Symbol "," : rest) =
    let (more, rest') = extractParams rest
    in (param : more, rest')
extractParams (Ident param : Symbol ")" : rest) = ([param], rest)
extractParams (Symbol ")" : rest) = ([], rest)
extractParams tokens = ([], tokens)

parseParams :: [Token] -> ([String], [Token])
parseParams (Symbol "(" : rest) =
    extractParams rest
parseParams tokens = ([], tokens)

-----------------
-- expressions --
-----------------

parseExpr0 :: [Token] -> (Term, [Token])
parseExpr0 tokens = 
    let (term, rest) = parseExpr1 tokens in
    case rest of
        (Symbol "+" : rest') ->
            let (term', rest'') = parseExpr0 rest' in
            missing "Add"
        _ -> (term, rest)

parseExpr1 :: [Token] -> (Term, [Token])
parseExpr1 (Symbol "-" : rest) =
    let (term, rest') = parseExpr2 rest in
    missing "Negate"
parseExpr1 tokens = parseExpr2 tokens

parseExpr2 :: [Token] -> (Term, [Token])
parseExpr2 (Num n : rest) = missing "Const"
parseExpr2 (Ident name : rest) = missing "Variable Reference"
parseExpr2 (Symbol "(" : rest) =
    let (term, rest') = parseTerm rest in
    case rest' of
        (Symbol ")" : rest'') -> (term, rest'')
        _ -> error ("Expected closing parenthesis, but got: " ++ show (take 10 rest'))
parseExpr2 tokens = error ("Unexpected tokens in expression: " ++ show (take 10 tokens))


---------------
-- ParseTerm --
---------------

parseTerm :: [Token] -> (Term, [Token])

-- fun
parseTerm (Keyword "fun" : Ident name : rest) =
    let (params, rest') = parseParams rest in
    let (body, rest'') = parseTerm rest' in
    missing "fun"

-- block
parseTerm (Symbol "{" : rest) =
    let (terms, rest') = parseTerms (Just (Symbol "}")) rest in
    missing "{ ... }"

-- print
parseTerm (Keyword "print" : rest) =
    let (arg, rest') = parseTerm rest in
    missing "print"

-- if
parseTerm (Keyword "if" : rest) =
    let (cond, rest') = parseTerm rest in
    let (thenBranch, rest'') = parseTerm rest' in
    case rest'' of
        (Keyword "else" : rest''') ->
            let (elseBranch, rest'''') = parseTerm rest''' in
            missing "if ... else ..."
        _ -> missing "if ..."

-- while
parseTerm (Keyword "while" : rest) =
    let (cond, rest') = parseTerm rest in
    let (body, rest'') = parseTerm rest' in
    missing "while"

-- var declaration
parseTerm (Keyword "var" : Ident name : rest) =
    case rest of
        (Symbol "=" : rest') ->
            let (initExpr, rest'') = parseTerm rest' in
            missing "var ... = ..."
        _ -> missing "var ..."


-- expression
parseTerm tokens = parseExpr0 tokens

----------------
-- parseTerms --
----------------

parseTerms :: Maybe Token -> [Token] -> ([Term], [Token])
parseTerms Nothing [] = ([], [])
parseTerms (Just x) [] = error ("Expected closing token: " ++ show x ++ ", but got end of input")
parseTerms (Just x) (c:rest) | c == x = ([], rest) 
parseTerms end tokens =
    let (term, rest) = parseTerm tokens in
    let (terms, rest') = parseTerms end rest in
    (term : terms, rest')


parse :: String -> (Term, [Token])
parse s = let tokens = lexer s in
    let (terms, rest) = parseTerms Nothing tokens
    in missing "Top-level terms"