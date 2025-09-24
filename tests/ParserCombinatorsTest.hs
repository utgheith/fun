module ParserCombinatorsTest(runTests) where

import Control.Monad.State.Lazy (runStateT)

import Test.HUnit

import ParserCombinators


assertions :: [Assertion]
assertions = [
    assertEqual "" (runStateT eof "") (Right ((), ""))
    ]


tests :: Test
tests = TestList [
    TestCase a | a <- assertions
    ]

runTests :: IO Counts
runTests = runTestTT tests