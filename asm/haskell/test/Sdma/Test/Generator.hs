{-# LANGUAGE OverloadedStrings #-}
--
--
module Sdma.Test.Generator where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Test.HUnit hiding (Label)
import Sdma
import Sdma.Parser
import Sdma.Codegen
import Sdma.Cpp
import Data.Word
import Data.List

--import Debug.Trace

spi = initSourcePosInfo "(file)"

testBranch opc expect = testInstruction' "1: " (opc ++ " 1b") expect
testInstruction = testInstruction' ""
testInstruction' label insn expect = TestCase $ Right [expect] @=?
  assembleFile "(file)" (sToBs (label ++ insn)) Nothing spi

testPass1 str expect = TestCase $ pass1 $ parseSdmaAsm "(file)" str
  where pass1 = either parserFail check
        check insn = expect @=? fixLabels (WordAddr Rel 0) insn

parserFail = assertString . ("parse error: " ++) . show


testGen :: String -> [Word16] -> Test
testGen str expect = TestCase $
    assembleFile "(file)" (sToBs str) (Just 0x400) spi @?= Right expect

testGenRel :: String -> [Word16] -> Test
testGenRel str expect = TestCase $
    assembleFile "(file)" (sToBs str) Nothing spi @?= Right expect

testGenAbs :: Word16 -> String -> [Word16] -> Test
testGenAbs addr str expect = TestCase $
    assembleFile "(file)" (sToBs str) (Just addr) spi @?= Right expect

testGenError :: Maybe Word16 -> String -> String -> Test
testGenError addr src expect = TestCase $
    case assembleFile "(file)" (sToBs src) addr spi of
      Left msg -> assertBool ("expected error message containing \"" ++ expect ++ "\", but got " ++ msg)
                             (expect `isInfixOf` msg)
      Right w -> assertFailure $ "assemble error expected, but succeeded: " ++ (show w)



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
                     , testGen "1:.dc.w 1b-.\n2:.dc.w 2b-1b\n3:.dc.w 1f-.\n1:bdf 2b"
                               [0x0000, 0x0001, 0x0001, 0x7ffd]
                     , testGenAbs 0x1700 "1: 2: .dc.w 1b, 2b, 1b-2b"
                                         [0x1700, 0x1700, 0]
                     , testGenError Nothing "1: .dc.w foo" "foo: not defined"
                     , testGenError Nothing "99: .dc.w 42f" "local label 42: not found"
                     , testGenAbs 0x400 "1: .dc.w 1b" [0x0400]
                     , testGenError (Just 0x400) "1: .dc.w 1f" "local label 1: not found"
                     , testGenError (Just 0x400) "jmp exit" "exit: not defined"
                     , testGenError (Just 0x400) "jmp 3f" "local label 3: not found"
                     ]
        , TestLabel "relative jump" $
            TestList [ testGen "label1: add r0, r0\nlabel2: bf label2\nlabel3: bf label1"
                                [0x0098, 0x7cff, 0x7cfd]
                     , testGen "loop exit, 0\nadd r0, r0\nadd r0, r0\nexit: "
                                [0x7802, 0x0098, 0x0098]
                     , testGenError Nothing "1: loop foo" "foo: not defined"
                     , testGenError Nothing "1: bf label1" "label1: not defined"
                     , testGenError Nothing "2: bf 3f" "local label 3: not found"
                     ]
        , TestLabel "directives" $
            TestList [ testInstruction ".dc 0xabcd" 0xabcd
                     , testInstruction ".dc.w 0xabcd" 0xabcd
                     , testGen ".dc.b 0x12, 0x13, 0x14, 0x15" [0x1213, 0x1415] -- Big endian
                     , testGen ".dc.b 0x12, 0x13, 0x14" [0x1213, 0x1400]
                     , testGen "1: .dc.w 0x1234, 1b-." [0x1234, 0xffff]
                     , testGen ".dc.l 42" [0, 42]
                     , testGen ".dc.l 0xdeadbeef" [0xdead, 0xbeef]
                     , testGen ".dc.b 1, 2, 3\n.dc.b 4, 5, 6, 7\n.dc.w 8"
                               [0x0102, 0x0304, 0x0506, 0x0700, 0x0008]  -- auto alignment
                     , testGen ".dc.b 1, 2, 3;.dc.w 4"
                               [0x0102, 0x0300, 0x0004] -- auto alignment
                     , testGenError Nothing ".dc.b 1 ;.dc.l 0x12345678"
                                    "not aligned to 2-word boundary"
                     , testGenError Nothing ".dc.w 1 ;.dc.l 0x12345678"
                                    "not aligned to 2-word boundary"
                     , testGenAbs 0x0401 ".dc.w 0xabab\n.dc.l 0xdeadbeef\n"
                                  [0xabab, 0xdead, 0xbeef]
                     , testGenError Nothing "1: 2: .align 2,4" "too many"
                     , testGenError Nothing "1: .align 1b" "labels are not allowed"
                     , testGenError Nothing "label1: label2: .align label1-label2" "labels are not allowed"
                     ]
        , TestLabel "keep track with code address" $
            TestList [ testGen (  "1: bt 99f\n"
                               ++ "   .dc.b 1\n"
                               ++ "   .dc.l 0xdeadbeef\n"
                               ++ "99:")
                               [0x7d03, 0x0100, 0xdead, 0xbeef]
                     , testGen (  "1: bt 99f\n"
                               ++ "   .dc.w 1, 2\n"
                               ++ "99:")
                               [0x7d02, 0x0001, 0x0002]
                     , testGen (  "1: bt 99f\n"
                               ++ "   .dc.b 1, 2, 3, 4\n"
                               ++ "99:")
                               [0x7d02, 0x0102, 0x0304]
                     , testGen (  "1: bt 99f\n"
                               ++ "   .dc.b 1, 2, 3\n"
                               ++ "99:")
                               [0x7d02, 0x0102, 0x0300]
                     , testGen (  "1: bt 99f\n"
                               ++ "   .dc.w 0xabcd\n"
                               ++ "   .dc.l 0x12345678\n"
                               ++ "99:")
                               [0x7d03, 0xabcd, 0x1234, 0x5678]
                     , testGen (  "  bt 99f\n"
                               ++ "  .dc.w 1, label-.\n"
                               ++ "  .align 2\n"
                               ++ "label: .dc.l 0xdeadbeef\n"
                               ++ "99:")
                               [0x7d05, 0x0001, 0x0002, 0x0000, 0xdead, 0xbeef]
                     , testGenAbs 0x1801 (  "  .dc.w label1\n"
                                         ++ "  .align 2*2\n"
                                         ++ "label1:\n")
                                         [0x1804]
                     ]
        , TestLabel "relocatable / absolute address" $
            TestList [ testGenRel (  "1: bt 2f\n"
                                  ++ "   .dc.b 1, 2, 3\n"
                                  ++ "2:\n"
                                  ++ "   .dc.w 0xdead\n"
                                  ++ "   .dc.w 2b-2f\n"
                                  ++ "2:")
                                  [0x7d02, 0x0102, 0x0300, 0xdead, 0xfffe]
                     , testGenAbs 0x1700 (  "1: bt 99f\n"
                                         ++ "   .dc.w 1, 2\n"
                                         ++ "   jmp 99f\n"
                                         ++ "   .dc.w 99f\n"
                                         ++ "99:")
                                  [0x7d04, 0x0001, 0x0002, 0x9705, 0x1705]
                     , testGenRel "jmp 0x400" [0x8400]
                     , testGenAbs 0x500 "jmp 0x400" [0x8400]
                     -- error for now
                     , testGenError Nothing "1: jmp 1b\n" "relocatable address"
                     , testGenError Nothing "label: .dc.w label\n" "relocatable address"
                     ]
        ]


--
-- Local Variables:
-- coding: utf-8
-- End:
