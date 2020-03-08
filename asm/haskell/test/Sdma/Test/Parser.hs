{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

--
--
module Sdma.Test.Parser where

import Test.HUnit hiding (Label)
import Sdma.Parser
import Data.Word
import Sdma.Base

--testExpr str expect = TestCase $ Right expect @=? (parseOperand "" str)


getOperand (AsmLine _ (Just (Statement _ [expr]))) = expr

testExpr str expect = TestCase $ Right expect @=?
    (parseSdmaAsm "" (sToBs ("op " ++ str)) >>= return . getOperand . head)

--
-- ignore source position to make writing tests easier
--
testExprS str expect = TestCase $ Right expect @=?
    (parseSdmaAsm "" (sToBs ("op " ++ str)) >>= return . simplify . getOperand . head)


--[AsmLine labels Nothing]
testEmptyLine :: String -> [WithPos Label] -> Test
testEmptyLine str labels = TestCase $ Right [AsmLine labels Nothing] @=? (parseSdmaAsm "" (sToBs str))
testLine :: String -> [WithPos Label] -> WithPos String -> [AsmExpr] -> Test
testLine str labels insn oprs = TestCase $ Right [(AsmLine labels (Just $ Statement insn oprs))] @=? (parseSdmaAsm "" (sToBs str))

data SimplifiedExpr
  = Leaf' AsmToken
  | UnaryExpr' String SimplifiedExpr
  | BinaryExpr' String SimplifiedExpr SimplifiedExpr
  | Indexed' SimplifiedExpr SimplifiedExpr
  | Register' Word8
  deriving (Eq, Show)

simplify :: AsmExpr -> SimplifiedExpr
simplify expr = case expr of
               Leaf (WithPos _ t) -> Leaf' t
               UnaryExpr (WithPos _ op) e -> UnaryExpr' op (simplify e)
               BinaryExpr (WithPos _ op) l r -> BinaryExpr' op (simplify l) (simplify r)
               Indexed r d -> Indexed' (simplify r) (simplify d)
               Register (WithPos _ w) -> Register' w

testParser :: Test
testParser = TestList
        [ TestLabel "test for tokens" $
            TestList
            [
              TestCase $ tokenize "" "+" @?= [WithPos 0 (Symbol "+")]
            , TestCase $ tokenize "" "   -   " @?= [WithPos 3 (Symbol "-")]
            , TestCase $ tokenize "" "  \n\r\n  \n" @?= [ WithPos 2 Eol
                                                        , WithPos 7 Eol
                                                        ]
            , TestCase $ tokenize "" "  abc ; ._xyz01" @?= [ WithPos 2 (Identifier "abc")
                                                           , WithPos 6 (Symbol ";")
                                                           , WithPos 8 (Identifier "._xyz01")
                                                       ]
            , TestCase $ tokenize ""  "   0xabcd" @?= [WithPos 3 (Number 0xabcd)]
            , TestCase $ tokenize ""  "123*0b01011111" @?= [ WithPos 0 (Number 123)
                                                           , WithPos 3 (Symbol "*")
                                                           , WithPos 4 (Number 0x5f)]
            -- comment
            , TestCase $ tokenize ""  "123  # abc  \ndef" @?= [ WithPos 0 (Number 123)
                                                              , WithPos 12 Eol
                                                              , WithPos 13 (Identifier "def")
                                                              ]
            , TestCase $ tokenize "" "123@xxx\n42" @?= [ WithPos 0 (Number 123)
                                                       , WithPos 3 (Unknown '@')
                                                       , WithPos 7 Eol
                                                       , WithPos 8 (Number 42)
                                                       ]
            ]
        , TestLabel "test for expressions" $
            TestList [ testExpr "xyz" (Leaf (WithPos 3 (Identifier "xyz")))
                     , testExpr " 123" (Leaf (WithPos 4 (Number 123)))
                     , testExprS "0xabcd" (Leaf' (Number 0xabcd))
                     , testExpr "+3" (UnaryExpr (WithPos 3 "+") (Leaf (WithPos 4 (Number 3))))
                     , testExprS " + abc" (UnaryExpr' "+" (Leaf' (Identifier "abc")))
                     , testExprS "1+3" (BinaryExpr' "+"
                                        (Leaf' (Number 1))
                                        (Leaf' (Number 3))
                                       )
                     , testExprS "5*-abc" (BinaryExpr' "*"
                                          (Leaf' (Number 5))
                                          (UnaryExpr' "-" (Leaf' (Identifier "abc")))
                                          )
                     , testExpr "3f-5b" (BinaryExpr (WithPos 5 "-")
                                          (Leaf (WithPos 3 (LocalLabelRef Forward 3)))
                                          (Leaf (WithPos 6 (LocalLabelRef Backward 5)))
                                         )
                     , testExprS "3 + 5 * (.xx_y-2)" (BinaryExpr' "+"
                                                      (Leaf' (Number 3))
                                                      (BinaryExpr' "*"
                                                       (Leaf' (Number 5))
                                                       (BinaryExpr' "-"
                                                        (Leaf' (Identifier ".xx_y"))
                                                        (Leaf' (Number 2))))
                                                     )
                     , testExprS "  ( .xx_y-2 ) % 5 " (BinaryExpr' "%"
                                                       (BinaryExpr' "-"
                                                        (Leaf' (Identifier ".xx_y"))
                                                        (Leaf' (Number 2)))
                                                       (Leaf' (Number 5))
                                                      )
                     , testExprS "0xdeadbeef" (Leaf' (Number 0xdeadbeef))
                     ]
        , TestLabel "test for statement" $
            TestList [ testEmptyLine "" []
                     , testEmptyLine " label1: label2:label3:   # comment"
                                     [(WithPos 1  (Label "label1")),
                                      (WithPos 9 (Label "label2")),
                                      (WithPos 16 (Label "label3"))]
                     , testEmptyLine "10: 11:123:   # comment"
                                     [(WithPos 0 (LocalLabel 10)),
                                      (WithPos 4 (LocalLabel 11)),
                                      (WithPos 7 (LocalLabel 123))]

                     , testLine "ret" [] (WithPos 0 "ret") []
                     , testLine "rorb r5" [] (WithPos 0 "rorb")
                                [Register (WithPos 5  5)]
                     , testLine "addi r4, 10" [] (WithPos 0 "addi")
                                [ Register (WithPos 5 4)
                                , Leaf (WithPos 9 (Number 10))
                                ]

                     , testLine "ld   r1, (r2, -4)" [] (WithPos 0 "ld")
                                [ Register (WithPos 5 1)
                                , Indexed (Register (WithPos 10 2))
                                          (UnaryExpr (WithPos 14 "-")
                                           (Leaf (WithPos 15 (Number 4))))
                                ]
                     , testLine "addi r7, foo + 3*(bar-5)" [] (WithPos 0 "addi")
                                [ Register (WithPos 5 7)
                                , BinaryExpr (WithPos 13 "+")
                                  (Leaf (WithPos 9 (Identifier "foo")))
                                  (BinaryExpr (WithPos 16 "*")
                                   (Leaf (WithPos 15 (Number 3)))
                                   (BinaryExpr (WithPos 21 "-")
                                    (Leaf (WithPos 18 (Identifier "bar")))
                                    (Leaf (WithPos 22 (Number 5))))
                                  )
                                ]

                     , testLine "label: ret" [(WithPos 0 (Label "label"))]
                                             (WithPos 7 "ret") []
                     ]
        ]


--
-- Local Variables:
-- coding: utf-8
-- End:
