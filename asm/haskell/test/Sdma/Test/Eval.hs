--
--
module Sdma.Test.Eval where

import Test.HUnit hiding (Label)
import Data.Word
import Data.Bits
import Sdma
import Sdma.Base
import Sdma.Parser
import Sdma.Codegen
import Sdma.Cpp


spi = initSourcePosInfo "(file)"

testExpr :: String -> Word32 -> Test
testExpr str expect = TestLabel str $ TestCase $
    either assembleFailure checkResult $ assembleFile "(file)" (sToBs (".dc.l " ++ str)) (Just 0x400) spi

  where
    assembleFailure msg = assertFailure msg
    checkResult ws = do
        assertEqual ("2 words should generated but got" ++ show ws) (length ws) 2
        wtolw ws @?= expect

testEvaluator :: Test
testEvaluator = TestList
        [ TestLabel "test arithmetic expressions" $
            TestList
            [
              testExpr "123" 123
            , testExpr "-123" 0xffffff85
            , testExpr "1+2 + 3" 6
            , testExpr "1+5-2" 4
            , testExpr "3*8" 24
            , testExpr "5/2" 2
            , testExpr "5%2" 1
            , testExpr "1+10*2" 21
            , testExpr "(1+10)*2" 22
            ]
        , TestLabel "test bitwise operations" $
            TestList
            [ testExpr "0x12345678 | 0x0f0f0f0f" 0x1f3f5f7f
            , testExpr "0x12345678 & 0x0f0f0f0f" 0x2040608
            , testExpr "~0x12346789" 0xedcb9876
            , testExpr "0x12345678 ^ 0x0f0f0f0f" 0x1d3b5977
            , testExpr "0x12345678 << 4" 0x23456780
            , testExpr "0x12345678 >> 4" 0x1234567
            , testExpr "0x00f0 + 1 << 8" 0xf100
            , testExpr "0x00f0 + (1 << 8)" 0x1f0
            , testExpr "0x00f0 << 1 + 8" 0x1e000
            , testExpr "0xf000 + 1 >> 8" 0xf0
            , testExpr "0x1234 & 0xff00 | 0x0033" 0x1233
            , testExpr "0x1234 & 0xff00 ^ 0xff00" 0xed00
            , testExpr "0x1234 & 0x00ff << 8" 0x1200
            ]
        ]


wtolw :: [Word16] -> Word32
wtolw (wh:wl:_) = (shift (toWord32 wh) 16) .|. (toWord32 wl)

--
-- Local Variables:
-- coding: utf-8
-- End:
