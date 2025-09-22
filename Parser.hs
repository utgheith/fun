{-# LANGUAGE LambdaCase #-}


module Parser where


import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Lazy (get, put, runState, StateT (runStateT))
import Lexer (lexer, Token(Ident, Keyword, Symbol))

----------- Result -----------

type Result = Either String

----------- Parser -----------

type Parser = StateT [Token] Result

eof :: Parser ()
eof = do
    tokens <- get
    case tokens of
      [] -> return ()
      _  -> throwError "expected eof"

satisfy :: (Token -> Maybe a) -> Parser a
satisfy p = do
    tokens <- get
    case tokens of
      [] -> throwError "out of tokens"
      (t:rest) -> do
        case p t of
          Just a -> do
            put rest
            return a
          Nothing -> throwError "mismatch"

token :: Token -> Parser Token
token t = satisfy $ \x -> if x == t then Just t else Nothing

(<|>) :: Parser a -> Parser b -> Parser (Either a b)
p1 <|> p2 = catchError
  (Left <$> p1)
  (\_ -> Right <$> p2)

oneof :: [Parser a] -> Parser a
oneof [] = throwError "no choices left"
oneof (p:ps) = fmap
  (\case
    (Left a) -> a
    (Right a) -> a)
  (p <|> oneof ps)

maybe :: Parser a -> Parser (Maybe a)
maybe p = catchError
  (Just <$> p)
  (const $ return Nothing)

rpt :: Parser a -> Parser [a]
rpt p = catchError
  (do
      x <- p
      xs <- rpt p
      return (x:xs))
  (const $ return [])

rptsep :: Parser a -> String -> Parser [a]
rptsep p sep = catchError
  (do
      x <- p
      xs <- rpt $ do
        exact [sep]
        p
      return (x:xs))
  (const $ return [])

---------- fun syntax -----------

exact :: [String] -> Parser String
exact candidates = satisfy $ \case
  (Keyword s) | s `elem` candidates -> Just s
  (Symbol s) | s `elem` candidates -> Just s
  (Ident s) | s `elem` candidates -> Just s
  _ -> Nothing

ident :: Parser String
ident = satisfy $ \case
  (Ident id) -> Just id
  _ -> Nothing

