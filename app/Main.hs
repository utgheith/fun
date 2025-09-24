module Main where

import FunDecompile (decompile)
import FunSyntax (parse, prog)
import ParserCombinators (eof)



main :: IO ()
main = do
    text <- getContents
    let r = parse text $ do
            t <- prog
            _ <- eof
            return t
    case r of
        Left err -> print err
        Right (term, rest) -> do
            putStrLn $ decompile term
            print rest
            
