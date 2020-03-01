{-# LANGUAGE OverloadedStrings #-}
--
--
module Sdma.UnitTest where

import Test.HUnit hiding (Label)
--import System.Environment
import Sdma.Parser
import Text.Megaparsec

at :: Int -> Int -> Int -> a -> WithPos a
at line col len tok = WithPos (SourcePos "" (mkPos line) (mkPos col)) tok

eol :: Int -> Int -> Int -> WithPos AsmToken
eol line col _ = WithPos (SourcePos "" (mkPos line) (mkPos col)) Eol

testExpr str expect = TestCase $ Right expect @=? (parseExpr "" str)
--testLine str labels expect = TestCase $ Right (labels, expect, 1) @=? parseLine str

tests :: Test
tests = TestList
        [ TestLabel "test for tokens" $
            TestList
            [
              TestCase $ tokenize "" "+" @?= [at 1 1 1 (Symbol "+")]
            , TestCase $ tokenize "" "   -   " @?= [at 1 4 1 (Symbol "-")]
            , TestCase $ tokenize "" "  \n\r\n  \n" @?= [ WithPos (SourcePos "" (mkPos 1) (mkPos 3)) Eol
                                                        , WithPos (SourcePos "" (mkPos 3) (mkPos 3)) Eol
                                                    ]
            , TestCase $ tokenize "" "  abc ; ._xyz01" @?= [ at 1 3 3 (Identifier "abc")
                                                           , at 1 7 1 (Symbol ";")
                                                           , at 1 9 7 (Identifier "._xyz01")
                                                       ]
            , TestCase $ tokenize ""  "   0xabcd" @?= [at 1 4 6 (Number 0xabcd)]
            , TestCase $ tokenize ""  "123*0b01011111" @?= [ at 1 1 3 (Number 123)
                                                           , at 1 4 1 (Symbol "*")
                                                           , at 1 5 10 (Number 0x5f)]
            -- comment
            , TestCase $ tokenize ""  "123  # abc  \ndef" @?= [ at 1 1 3 (Number 123)
                                                              , eol 1 13 1
                                                              , at 2 1 3 (Identifier "def")
                                                              ]
            , TestCase $ tokenize "" "123@xxx\n42" @?= [ at 1 1 3 (Number 123)
                                                       , at 1 4 4 (Unknown '@')
                                                       , eol 1 8 1
                                                       , at 2 1 2 (Number 42)
                                                       ]
            ]
        , TestLabel "test for expressions" $
            TestList [ testExpr "xyz" (Leaf (at 1 1 3 (Identifier "xyz")))
                     , testExpr " 123" (Leaf (at 1 2 3 (Number 123)))
                     , testExpr "0xabcd" (Leaf (at 1 1 6 (Number 0xabcd)))
                     , testExpr "+3" (UnaryExpr (at 1 1 1 "+") (Leaf (at 1 2 1 (Number 3))))
                     , testExpr " + abc" (UnaryExpr (at 1 2 1 "+") (Leaf (at 1 4 1 (Identifier "abc"))))
                     , testExpr "1+3" (BinaryExpr (at 1 2 1 "+")
                                       (Leaf (at 1 1 1 (Number 1)))
                                       (Leaf (at 1 3 1 (Number 3)))
                                      )
                     , testExpr "5*-abc" (BinaryExpr (at 1 2 1 "*")
                                          (Leaf (at 1 1 1 (Number 5)))
                                          (UnaryExpr (at 1 3 1 "-") (Leaf (at 1 4 1 (Identifier "abc"))))
                                         )
                     , testExpr "3f-5b" (BinaryExpr (at 1 3 1 "-")
                                          (Leaf (at 1 1 1 (LocalLabelRef Forward 3)))
                                          (Leaf (at 1 4 1 (LocalLabelRef Backward 5)))
                                         )
                     , testExpr "3 + 5 * (.xx_y-2)" (BinaryExpr (at 1 3 1 "+")
                                                     (Leaf (at 1 1 1 (Number 3)))
                                                     (BinaryExpr (at 1 7 1 "*")
                                                      (Leaf (at 1 5 1 (Number 5)))
                                                      (BinaryExpr (at 1 15 1 "-")
                                                       (Leaf (at 1 10 1 (Identifier ".xx_y")))
                                                       (Leaf (at 1 16 1 (Number 2)))))
                                                    )
                     , testExpr "  ( .xx_y-2 ) % 5 " (BinaryExpr (at 1 15 1 "%")
                                                       (BinaryExpr (at 1 10 1 "-")
                                                        (Leaf (at 1 5 1 (Identifier ".xx_y")))
                                                         (Leaf (at 1 11 1 (Number 2))))
                                                       (Leaf (at 1 17 1 (Number 5)))
                                                     )
                     ]
        ]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()


--
-- Local Variables:
-- coding: utf-8
-- End:
