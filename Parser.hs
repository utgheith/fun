{-# LANGUAGE LambdaCase #-}


module Parser(eof, oneof, opt, Parser, Result, rpt, rptsep, satisfy, token) where


import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Lazy (get, put, StateT)
-- import Lexer (lexer, Token(Ident, Keyword, Symbol))

----------- Result -----------

type Result = Either String

----------- Parser -----------

type Parser t = StateT [t] Result

eof :: Parser t ()
eof = do
    tokens <- get
    case tokens of
      [] -> return ()
      _  -> throwError "expected eof"

satisfy :: (t -> Maybe a) -> Parser t a
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

token :: (Show t, Eq t) => t -> Parser t t
token t = do
  tokens <- get
  case tokens of
    [] -> throwError "out of tokens"
    (t':rest) -> if t == t' then do
                    put rest
                    return t
                 else
                    throwError ("expected " ++ show t ++ ", got " ++ show t')

(<|>) :: Parser t a -> Parser t b -> Parser t (Either a b)
p1 <|> p2 = catchError
  (Left <$> p1)
  (\_ -> Right <$> p2)

oneof :: [Parser t a] -> Parser t a
oneof [] = throwError "no choices left"
oneof (p:ps) = fmap
  (\case
    (Left a) -> a
    (Right a) -> a)
  (p <|> oneof ps)

opt :: Parser t a -> Parser t (Maybe a)
opt p = catchError
  (Just <$> p)
  (const $ return Nothing)

rpt :: Parser t a -> Parser t [a]
rpt p = catchError
  (do
      x <- p
      xs <- rpt p
      return (x:xs))
  (const $ return [])

rptsep :: Parser t a -> Parser t b -> Parser t [a]
rptsep p sep = catchError
  (do
      x <- p
      xs <- rpt $ do
        _ <-sep
        p
      return (x:xs))
  (const $ return [])


