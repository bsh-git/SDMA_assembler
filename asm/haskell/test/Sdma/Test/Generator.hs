{-# LANGUAGE OverloadedStrings #-}
--
--
module Sdma.Test.Generator where

import Test.HUnit hiding (Label)
import Sdma
import Sdma.Parser
import Sdma.Codegen
import Data.Text
import Data.Word
--import qualified Data.Text.IO as Txtio

--import Debug.Trace

testBranch opc expect = testInstruction' "1: " (opc ++ " 1b") expect
testInstruction = testInstruction' ""
testInstruction' label insn expect = TestCase $ Right [expect] @=?
  assembleFile "(file)" (pack (label ++ insn)) 0

testPass1 str expect = TestCase $ pass1 $ parseSdmaAsm "(file)" str
  where pass1 = either parserFail check
        check insn = expect @=? fixLabels (WordAddr 0) insn

testGen :: String -> [Word16] -> Test
testGen str expect = TestCase $
    assembleFile "(file)" (pack str) 0 @?= Right expect

parserFail = assertString . ("parse error: " ++) . show
--codegenFail = assertString . ("assemble error: " ++) . show



testGenerator :: Test
testGenerator = TestList
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
                     , testInstruction "loop 10,3"      0x7b0a
                     , testInstruction "lsl1 r5"        0x0517
                     , testInstruction "lsr1 r5"        0x0515
                     , testInstruction "mov  r6, r7"    0x068f
                     , testInstruction "notify 3"       0x0301
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
                                 [(2, [WithPos 32 (LocalLabel 1)]),
                                  (1, [WithPos 11 (Label "label")])]
                     , testPass1 "label:add r1, r3\n\nfoo: bar: addi r0, 0xdb\n"
                                 [ (1, [ WithPos 18 (Label "foo")
                                       , WithPos 23 (Label "bar")])
                                 , (0, [WithPos 0 (Label "label")])]
                     , testPass1 "label1: # comment\nlabel2:\nadd r1, r3\n\nfoo: bar: addi r0, 0xdb\n"
                                 [ (1, [ WithPos 38 (Label "foo")
                                       , WithPos 43 (Label "bar")])
                                 , (0, [ WithPos 18 (Label "label2")
                                       , WithPos 0 (Label "label1")])
                                 ]
                     , testGen "1:.dc 1b-.\n2:.dc 2b-1b\n3:.dc 1f-.\n1:bdf 2b"
                               [0x0000, 0x0001, 0x0001, 0x7ffd]
                     ]
        , TestLabel "relative jump" $
            TestList [ testGen "label1: add r0, r0\nlabel2: bf label2\nlabel3: bf label1"
                                [0x0098, 0x7cff, 0x7cfd]
                     , testGen "loop exit, 0\nadd r0, r0\nadd r0, r0\nexit: "
                                [0x7802, 0x0098, 0x0098]
                     ]
        , TestLabel "directives" $
            TestList [ testInstruction ".dc 0xabcd" 0xabcd
                     , testInstruction ".dc.w 0xabcd" 0xabcd
                     , testGen ".dc.b 0x12, 0x13, 0x14, 0x15" [0x1213, 0x1415] -- Big endian
                     , testGen ".dc.b 0x12, 0x13, 0x14" [0x1213, 0x1400]
                     , testGen "1: .dc 0x1234, 1b-." [0x1234, 0xffff]
                     , testGen ".dc.l 42" [0, 42]
                     , testGen ".dc.l 0xdeadbeef" [0xdead, 0xbeef]
                     , testGen ".dc.b 1, 2, 3\n.dc.b 4, 5, 6, 7\n.dc.w 8"
                               [0x0102, 0x0304, 0x0506, 0x0700, 0x0008]  -- auto alignment
                     , testGen ".dc.b 1, 2, 3;.dc 4"
                               [0x0102, 0x0300, 0x0004] -- auto alignment
                     , testGen ".dc.b 1 ;.dc.l 0x12345678"
                               [0x0100, 0x1234, 0x5678] -- aliigned to word baundary, not long word
                     ]
        , TestLabel "keep track with code address" $
            TestList [ testGen (  "1: bt 99f\n"
                               ++ "   .dc.b 1, 2, 3\n"
                               ++ "   .dc.l 0xdeadbeef\n"
                               ++ "99:")
                               [0x7d04, 0x0102, 0x0300, 0xdead, 0xbeef]
                     , testGen (  "1: bt 99f\n"
                               ++ "   .dc.w 1, 2\n"
                               ++ "99:")
                               [0x7d02, 0x0001, 0x0002]
                     ]
        ]


--
-- Local Variables:
-- coding: utf-8
-- End:
