--
--
module Sdma.UnitTest where

import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import Sdma
import Sdma.Read


parseExpr str = parse asmExpression "(file)" str
parseOperand str = parse asmOperand "(file)" str

testExpr str expect = TestCase $  Right expect @=? parse asmExpression "(file)" str
testLine str labels expect = TestCase $ Right (labels, expect, 1) @=? parse asmLine "(file)" str

tests :: Test
tests = TestList
        [ TestLabel "Test for parsing a term" $
            TestList [ testExpr "xyz" (Symbol "xyz")
                     , testExpr "123" (Number 123)
                     , testExpr "0xabcd" (Number 0xabcd)
                     -- not octal
                     , testExpr "010" (Number 10)
                     , TestCase (do
                                    let Right e = parseExpr "r3"
                                    case e of
                                      Symbol _ ->
                                        case symbolToRegister e of
                                          Register r -> r @?= 3
                                          _ -> assertFailure "register r3"
                                      _ -> assertFailure "register r3")
                     , testExpr "(125)" (Number 125)
                     , testExpr "-5" (UnaryOp "-" (Number (5)))
                     ]
        , TestLabel "test for (reg, disp)" $
            TestList [ TestCase (do
                                    let Right e = parseOperand "(r5, 32)"
                                    case e of
                                      Indexed reg (Number disp) ->
                                        reg == 5 && disp == 32 @? "(r5, 32)" ++ (show e)
                                      _ -> assertFailure "(r5, 32)")

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
                     ]
        , TestLabel "test multiple lines" $
            TestList [ TestCase (do
                                    let Right lns = parse asmFile "(file)"
                                                            "label: ldr r0, (r3, 0)\njsr subroutine"
                                    lns @?= [(["label"], Just $ SdmaInstruction "ldr" (Symbol "r0") (Indexed 3 (Number 0)),1),
                                              ([],Just $ SdmaInstruction "jsr" (Symbol "subroutine") Empty,2)])
                     , TestCase (do
                                    let Right lns = parse asmFile "(file)"
                                                    "\nL: ret\n\nclr"
                                    lns @?= [([],Nothing,1),
                                             (["L"],Just $ SdmaInstruction "ret" Empty Empty, 2),
                                             ([], Nothing, 3),
                                             ([], Just $ SdmaInstruction "clr" Empty Empty, 4)])
                     , TestCase (do
                                    let Right lns = parse asmFile "(file)"
--                                                      "a\nb\nc\nd\n"
--                                                    "start:\n\n     ldi r0, 4\n\nloop exit, 0"
--                                                    "start:\n\n     ldi r0, 4\n\nloop exit, 0"
                                                    "ldi r0, 0\n\nloop exit, 0\nstart: cli"
                                    lns @?= [])
                     ]
          ]



main :: IO ()
main = do
    runTestTT tests >>  getArgs >>= mapM_ parseFile

  where
    parseFile file = do
      putStrLn $ "parse " ++ file
      lines <- readFile file
      either (putStr . show) showResults $ parseLines file lines
    showResults  = mapM_ (putStrLn . show)


--
-- Local Variables:
-- coding: utf-8
-- End:
