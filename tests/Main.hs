module Main where


import qualified ParserCombinatorsTest as PCT


main :: IO ()
main = do
    c <- PCT.runTests
    print c