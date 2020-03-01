{-# LANGUAGE OverloadedStrings #-}
--
--
module Sdma.TestGenerator where

import Test.HUnit hiding (Label)
import System.Environment
import Sdma.Parser
import Sdma.Codegen
import Data.Either.Combinators (fromRight')
import Data.Text
import qualified Data.Text.IO as Txtio

import Debug.Trace

at :: Int -> Int -> Int -> a -> WithPos a
at line col _ tok = WithPos (SourcePos "(file)" (mkPos line) (mkPos col)) tok

testBranch opc expect = testInstruction' "1: " (opc ++ " 1b") expect
testInstruction = testInstruction' ""
testInstruction' label insn expect = TestCase $ gen $ parseSdmaAsm "(file)" $ pack (label ++ insn)
  where gen = either parserFail check
        check lns = assertEqual ("test " ++ insn) (Right [expect]) $ generate 0 [(0, [at 1 1 1 (LocalLabel 1)])] lns

testPass1 str expect = TestCase $ pass1 $ parseSdmaAsm "(file)" str
  where pass1 = either parserFail check
        check insn = expect @=? fixLabels 0 insn

parserFail = assertString . ("parse error: " ++) . show
--codegenFail = assertString . ("assemble error: " ++) . show


tests :: Test
tests = TestList
        [ TestLabel "Instructions" $
            TestList [ testInstruction "add r1, r3"     0x019b
                     , testInstruction "addi r0, 0xdb"  0x18db
                     , testInstruction "and r2, r4"     0x02bc
                     , testInstruction "andi r3, 16"    0x3b10
                     , testInstruction "andn r4, r5"    0x04b5
                     , testInstruction "andni   r5, 33" 0x3521
                     , testInstruction "asr1\tr6"       0x0616
                     , testInstruction "bclri r7, 12"   0x072c
                     , testInstruction "bclri r7, 24"   0x0738
                     , testBranch      "bdf"            0x7fff
                     , testBranch      "bf"             0x7cff
                     , testInstruction "bseti r0,31"    0x005f
                     , testBranch      "bsf"            0x7eff
                     , testBranch      "bt"             0x7dff
                     , testInstruction "btsti r1, 30"   0x017e
                     , testInstruction "clrf 3"         0x0307
                     , testInstruction "cmpeq r2, r3"   0x02cb
                     , testInstruction "cmpeqi r3, 127" 0x4b7f
                     , testInstruction "cmphs r4, r7"   0x04df
                     , testInstruction "cmplt r5, r6"   0x05d6
                     , testInstruction "cpshreg"        0x06e2
                     , testInstruction "done 3"         0x0300
                     , testInstruction "illegal"        0x0707
                     , testInstruction "jmp 0x1abc"     0x9abc
                     , testInstruction "jmpr r5"        0x0508
                     , testInstruction "jsr 0x1abc"     0xdabc
                     , testInstruction "jsrr r5  "      0x0509
                     , testInstruction "ld r1, (r2,4)"  0x5122
                     , testInstruction "ldf r2, 13"     0x620d
                     , testInstruction "ldi r3, 0xa5"   0x0ba5
                     , testInstruction "ldrpc r4"       0x040a
                     , testInstruction "loop 4,r3"      0x7b04
                     , testInstruction "lsl1 r5"        0x0417
                     , testInstruction "lsr1 r5"        0x0415
                     , testInstruction "mov  r6, r7"    0x068f
                     , testInstruction "nitify 3"       0x0301
                     , testInstruction "or r0, r7"      0x00af
                     , testInstruction "ori r1, 0x55"   0x2955
                     , testInstruction "ret"            0x0006
                     , testInstruction "revb r2"        0x0210
                     , testInstruction "revblo r3"      0x0311
                     , testInstruction "ror1 r4"        0x0414
                     , testInstruction "rorb r5"        0x0512
                     , testInstruction "softbkpt"       0x0005
                     , testInstruction "st r6,(r4,10)"  0x5e54
                     , testInstruction "stf r7, 0x2b"   0x6f2b
                     , testInstruction "sub r4, r7"     0x04a7
                     , testInstruction "subi r3, 0x55"  0x2355
                     , testInstruction "tst r2,r5"      0x02c5
                     , testInstruction "tsti r3,0xaa"   0x43aa
                     , testInstruction "xor r4, r7"     0x0497
                     , testInstruction "xori r4, 0xc3"  0x14c3
                     , testInstruction "xori r4, 0xc3"  0x14c3
                     , testInstruction "yeild"          0x0000   -- done 0
                     , testInstruction "yeildge"        0x0100   -- done 1
                     ]

        , TestLabel "Labels" $
            TestList [ testPass1 "add r1, r3\nlabel: addi r0, 0xdb\n1:and r2, r4"
                                 [(2, [at 3 1 0 (LocalLabel 1)]),
                                  (1, [at 2 1 0 (Label "label")])]
                     , testPass1 "label:add r1, r3\n\nfoo: bar: addi r0, 0xdb\n"
                                 [ (1, [ at 3 1 0 (Label "foo")
                                       , at 3 6 0 (Label "bar")])
                                 , (0, [at 1 1 0 (Label "label")])]
                     , testPass1 "label1: # comment\nlabel2:\nadd r1, r3\n\nfoo: bar: addi r0, 0xdb\n"
                                 [ (1, [ at 5 1 0 (Label "foo")
                                       , at 5 6 0 (Label "bar")])
                                 , (0, [ at 2 1 0 (Label "label2")
                                       , at 1 1 0 (Label "label1")])
                                 ]
                     ]
        , TestLabel "relative jump" $
            TestList [ TestCase $ do
                           let p = fromRight' $  parseSdmaAsm"(file)" "label1: add r0, r0\nlabel2: bf label2\nlabel3: bf label1"
                           generate 0 [] p @?= Right [0x0098, 0x7cff, 0x7cfe]
                     ]
        ]

main :: IO ()
main =
    runTestTT tests >>  getArgs >>= mapM_ processFile

  where
    processFile file = do
      putStrLn $ "parse " ++ file
      lns <- Txtio.readFile file
      either (putStr . show) gen $ parseSdmaAsm file lns
    gen insns = (putStr . show . (generate 0 (fixLabels 0 insns))) insns

--
-- Local Variables:
-- coding: utf-8
-- End:
