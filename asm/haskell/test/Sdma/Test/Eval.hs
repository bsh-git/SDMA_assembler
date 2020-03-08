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
testExpr str expect = TestCase $
    either assembleFailure checkResult $ assembleFile "(file)" (sToBs (".dc.l " ++ str)) (Just 0x400) spi

  where
    assembleFailure msg = assertFailure msg
    checkResult ws = do
        assertEqual ("2 words should generated but got" ++ show ws) (length ws) 2
        wtolw ws @?= expect

testEvaluator :: Test
testEvaluator = TestList
        [ TestLabel "test expression" $
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
        ]


wtolw :: [Word16] -> Word32
wtolw (wh:wl:_) = (shift (toWord32 wh) 16) .|. (toWord32 wl)

--
-- Local Variables:
-- coding: utf-8
-- End:
