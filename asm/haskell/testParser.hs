--
--
module Sdma.UnitTest where

import Test.HUnit
import System.Environment
import Sdma
import Sdma.Read
import Data.Either.Combinators (fromRight')


--parseExpr str = parse asmExpression "(file)" str
parseLine = head . fromRight' . (parseLines "(file)")
parseExpr str = let (_,Just (SdmaInstruction "OP" expr Empty),_) = parseLine $ "OP " ++ str
                in expr

testExpr str expect = TestCase $ expect @=? (parseExpr str)
testLine str labels expect = TestCase $ (labels, expect, 1) @=? parseLine str


tests :: Test
tests = TestList
        [ TestLabel "Test for parsing a term" $
            TestList [ testExpr "xyz" (Symbol "xyz")
                     , testExpr "123" (Number 123)
                     , testExpr "0xabcd" (Number 0xabcd)
                     -- not octal
                     , testExpr "010" (Number 10)
                     , testExpr "-5" (UnaryOp "-" (Number (5)))
                     , TestCase $ (Register 3) @?= symbolToRegister (parseExpr "r3")
                     ]
        , TestLabel "test for (reg, disp)" $
            TestList [ testExpr "(r5, 32)" (Indexed 5 (Number 32))
                        ]
        , TestLabel "test for binary op" $
            TestList [ testExpr "1+2" (BinaryOp "+" (Number 1) (Number 2))
                     , testExpr "1 + 2" (BinaryOp "+" (Number 1) (Number 2))
                     , testExpr "1 + -2" (BinaryOp "+" (Number 1) (UnaryOp "-" (Number 2)))
                     , testExpr "1 --20" (BinaryOp "-" (Number 1) (UnaryOp "-" (Number 20)))
                     , testExpr "1+2*3" (BinaryOp "+" (Number 1) (BinaryOp "*" (Number 2) (Number 3)))
                     ]
        , TestLabel "test for statement" $
            TestList [ testLine "ret" [] (Just $ SdmaInstruction "ret" Empty Empty)
                     , testLine "addi r4, 10" [] (Just $ SdmaInstruction "addi" (Symbol "r4") (Number 10))
                     , testLine "rorb r5" [] (Just $ SdmaInstruction "rorb" (Symbol "r5") Empty)
                     , testLine "ld  r0,(r1,4)" [] (Just $ SdmaInstruction "ld" (Symbol "r0")
                                                                                (Indexed 1 (Number 4)))
                     , testLine "label:bclri r0, 1" ["label"]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     , testLine "label: \tbclri r0, 1" ["label"]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     -- two labels at a line
                     , testLine "  label1: label2: bclri r0, 1" ["label1", "label2"]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     -- only a label
                     , testLine "  label: # bclri r0, 1" ["label"] Nothing
                     , testLine "op abc # comment" [] (Just $ SdmaInstruction "op" (Symbol "abc") Empty)
                     ]
        , TestLabel "test multiple lines" $
            TestList [ TestCase $ parseLines "(file)" "label: ldr r0, (r3, 0)\njsr subroutine\n"
                                  @?= Right [(["label"], Just $ SdmaInstruction "ldr" (Symbol "r0") (Indexed 3 (Number 0)),1),
                                             ([],Just $ SdmaInstruction "jsr" (Symbol "subroutine") Empty,2)]
                     , TestCase $ parseLines "(file)" "\nL: ret\n\nclr\n\nB:"
                                  @?= Right [(["L"],Just $ SdmaInstruction "ret" Empty Empty, 2),
                                             ([], Just $ SdmaInstruction "clr" Empty Empty, 4),
                                             (["B"], Nothing, 6)]
                     , TestCase $ parseLines "(file)" "ldi r0, 0\n\nloop exit, 0\nstart: cli"
                                  @?= Right [([], Just $ SdmaInstruction "ldi" (Symbol "r0") (Number 0), 1),
                                             ([], Just $ SdmaInstruction "loop" (Symbol "exit") (Number 0), 3),
                                             (["start"], Just $ SdmaInstruction "cli" Empty Empty, 4)]
                     ]
          ]



main :: IO ()
main = do
    runTestTT tests >>  getArgs >>= mapM_ parseFile

  where
    parseFile file = do
      putStrLn $ "parse " ++ file
      lns <- readFile file
      either (putStr . show) showResults $ parseLines file lns
    showResults  = mapM_ (putStrLn . show)


--
-- Local Variables:
-- coding: utf-8
-- End:
