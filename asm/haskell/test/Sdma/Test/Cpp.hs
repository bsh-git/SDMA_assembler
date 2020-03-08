{-# LANGUAGE OverloadedStrings #-}
--
--
module Sdma.Test.Cpp where

import Test.HUnit hiding (Label)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Void
import Data.Word
import Data.Set
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Sdma
import Sdma.Parser
import Sdma.Codegen
import Sdma.Cpp
import Text.Megaparsec.Error
import Text.Megaparsec.Pos


src1 = [ "# 1 \"error3.S\""
       , "# 1 \"<built-in>\""
       , "# 1 \"<command-line>\""
       , "# 31 \"<command-line>\""
       , "# 1 \"/usr/include/stdc-predef.h\" 1 3 4"
       , "# 32 \"<command-line>\" 2"
       , "# 1 \"error3.S\""
       , "# 1 \"defs.h\" 1"
       , "# 2 \"error3.S\" 2"
       , ""
       , "start:"
       , "     ldi r0, 4"
       , "     loop exit, 0"
       , " &&"
       , "     st r4, (r5, 0) # Address r5"
       , " ** xxx"
       , "     addi r5, -"
       , "     addi r4, 0x10"
       , "# 1 \"error4.S\" 1"
       , " addi r0, 5"
       , " jump foo"
       , " addi r9, 128"
       , "# 12 \"error3.S\" 2"
       , ""
       , "00ab: ex&it: # comment"
       , "     done 3 # comment"
       , "     addi r4, 0x40"
       , "     ldi r3, 0"
       , "     cmpeqi r3, 0 # Always true"
       , "     bt start # Always branches"
       , "42:"
       ]

input1 = B.unlines src1

sourcePos1 = [ SourcePosInfo 0 "(input)" 1
             , SourcePosInfo 15 "error3.S" 1
             , SourcePosInfo 32 "<built-in>" 1
             , SourcePosInfo 53 "<command-line>" 1
             , SourcePosInfo 75 "<command-line>" 31
             , SourcePosInfo 114 "/usr/include/stdc-predef.h" 1
             , SourcePosInfo 138 "<command-line>" 32
             , SourcePosInfo 153 "error3.S" 1
             , SourcePosInfo 168 "defs.h" 1
             , SourcePosInfo 185 "error3.S" 2
             , SourcePosInfo 323 "error4.S" 1
             ,  SourcePosInfo 377 "error3.S" 12
             ]             

error1 :: ParseErrorBundle ByteString Void
error1 = ParseErrorBundle 
              (NE.fromList
                        [ FancyError 187 (Data.Set.singleton (ErrorFail "error3.S"))
                        , FancyError 323 (Data.Set.singleton (ErrorFail "error4.S"))
                        , FancyError 325 (Data.Set.singleton (ErrorFail "error4.S"))
                        , FancyError 388 (Data.Set.singleton (ErrorFail "error3.S"))
                        ])
           PosState {pstateInput = "# 1 \"error3.S\"\n# 1 \"<built-in>\"\n# 1 \"<command-line>\"\n# 31 \"<command-line>\"\n# 1 \"/usr/include/stdc-predef.h\" 1 3 4\n# 32 \"<command-line>\" 2\n# 1 \"error3.S\"\n# 1 \"defs.h\" 1\n# 2 \"error3.S\" 2\n\nstart:\n     ldi r0, 4\n     loop exit, 0\n &&\n     st r4, (r5, 0) # Address r5\n ** xxx\n     addi r5, -\n     addi r4, 0x10\n# 1 \"error4.S\" 1\n addi r0, 5\n jump foo\n addi r9, 128\n# 12 \"error3.S\" 2\n\n00ab: ex&it: # comment\n     done 3 # comment\n     addi r4, 0x40\n     ldi r3, 0\n     cmpeqi r3, 0 # Always true\n     bt start # Always branches\n42:\n", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "error3.asm", sourceLine = mkPos 1, sourceColumn = mkPos 1}, pstateTabWidth = mkPos 8, pstateLinePrefix = ""}           

checkBundleFile :: ParseErrorBundle ByteString Void -> Assertion
checkBundleFile (ParseErrorBundle be ps) = do
    let (FancyError off s) = NE.head be
    let (ErrorFail f0) = head (toList s)
    let f1 = sourceName (pstateSourcePos ps)
    assertEqual "source file name" f0 f1
    -- XXX more check

testCpp :: Test
testCpp = TestList
        [ TestLabel "reading cpp marks" $
            TestList [ TestCase $ do r <- cppMarks "(input)" input1
                                     assertEqual "not equal" sourcePos1 r
                     ]
        , TestLabel "split ErrorBundle" $
            TestList [ TestCase $ do
                           let bs = splitErrorBundle sourcePos1 error1
                           assertEqual "the number of bundles" 4 (length bs)
                           mapM_ checkBundleFile bs
                     ]
        ]
        



--
-- Local Variables:
-- coding: utf-8
-- End:
