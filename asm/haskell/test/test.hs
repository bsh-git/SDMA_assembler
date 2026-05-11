import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Sdma.Test.Cpp
import Sdma.Test.Eval
import Sdma.Test.Generator
import Sdma.Test.Parser

-- An example test case
test1 :: Test
test1 = TestCase (assertEqual "should be 3" 3 (1 + 2))

-- Combine tests into a list
tests :: Test
tests = TestList [ testCpp
                 , testEvaluator
                 , testGenerator
                 , testParser
                 ]

main :: IO ()
main = do
    counts <- runTestTT tests
    -- Ensure stack test fails if any tests fail/error
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure
