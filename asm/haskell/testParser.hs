--
--
module Sdma.UnitTest where

import Test.HUnit hiding (Label)
import System.Environment
import Sdma
import Sdma.Read
import Data.Either.Combinators (fromRight')


--parseExpr str = parse asmExpression "(file)" str
parseLine lns = do
    result <- parseLines "(file)" lns
    return $ head result
parseExpr str = do
    result <- parseLines "(file)" ("OP " ++ str)
    case head result of
      (_,Just (SdmaInstruction "OP" expr Empty),_) -> return expr
      _ -> error $ "parser failed for expression " ++ str

testExpr str expect = TestCase $ Right expect @=? (parseExpr str)
testLine str labels expect = TestCase $ Right (labels, expect, 1) @=? parseLine str


tests :: Test
tests = TestList
        [ TestLabel "Test for parsing a term" $
            TestList [ testExpr "xyz" (Symbol "xyz")
                     , testExpr "123" (Number 123)
                     , testExpr "0xabcd" (Number 0xabcd)
                     -- not octal
                     , testExpr "010" (Number 10)
                     , testExpr "-5" (UnaryOp "-" (Number (5)))
                     , TestCase $ (Register 3) @?= symbolToRegister (fromRight' (parseExpr "r3"))
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
        , TestLabel "test for expression with ()" $
            TestList [ testExpr "(3*5)" (BinaryOp "*" (Number 3) (Number 5))
                     , testExpr "( 3 * 5 )" (BinaryOp "*" (Number 3) (Number 5))
                     , testExpr "3 * (a + 5)" (BinaryOp "*" (Number 3) (BinaryOp "+" (Symbol "a") (Number 5)))
                     , testExpr "3 * a + 5" (BinaryOp "+" (BinaryOp "*" (Number 3) (Symbol "a")) (Number 5))
                     ]
        , TestLabel "test for statement" $
            TestList [ testLine "ret" [] (Just $ SdmaInstruction "ret" Empty Empty)
                     , testLine "addi r4, 10" [] (Just $ SdmaInstruction "addi" (Symbol "r4") (Number 10))
                     , testLine "rorb r5" [] (Just $ SdmaInstruction "rorb" (Symbol "r5") Empty)
                     , testLine "ld  r0,(r1,4)" [] (Just $ SdmaInstruction "ld" (Symbol "r0")
                                                                                (Indexed 1 (Number 4)))
                     , testLine "label:bclri r0, 1" [Label "label"]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     , testLine "label: \tbclri r0, 1" [Label "label"]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     -- two labels at a line
                     , testLine "label1:label2: bclri r0, 1" [Label "label1", Label "label2"]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     , testLine "  1: 2: bclri r0, 1" [LocalLabel 1, LocalLabel 2]
                                (Just $ SdmaInstruction "bclri" (Symbol "r0") (Number 1))
                     -- only a label
                     , testLine "  label: # bclri r0, 1" [Label "label"] Nothing
                     , testLine "op abc # comment" [] (Just $ SdmaInstruction "op" (Symbol "abc") Empty)
                     -- local label
                     , testLine "1: jsr 3f" [LocalLabel 1] (Just $ SdmaInstruction "jsr" (LabelRef 3 Forward) Empty)
                     ]
        , TestLabel "test multiple lines" $
            TestList [ TestCase $ parseLines "(file)" "label: ldr r0, (r3, 0)\njsr subroutine\n"
                                  @?= Right [([Label "label"], Just $ SdmaInstruction "ldr" (Symbol "r0") (Indexed 3 (Number 0)),1),
                                             ([],Just $ SdmaInstruction "jsr" (Symbol "subroutine") Empty,2)]
                     , TestCase $ parseLines "(file)" "\nL: ret\n\nclr\n\nB:"
                                  @?= Right [([Label "L"],Just $ SdmaInstruction "ret" Empty Empty, 2),
                                             ([], Just $ SdmaInstruction "clr" Empty Empty, 4),
                                             ([Label "B"], Nothing, 6)]
                     , TestCase $ parseLines "(file)" "ldi r0, 0\n\nloop exit, 0\nstart: cli"
                                  @?= Right [([], Just $ SdmaInstruction "ldi" (Symbol "r0") (Number 0), 1),
                                             ([], Just $ SdmaInstruction "loop" (Symbol "exit") (Number 0), 3),
                                             ([Label "start"], Just $ SdmaInstruction "cli" Empty Empty, 4)]
                     ]
          ]



main :: IO ()
main =
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
