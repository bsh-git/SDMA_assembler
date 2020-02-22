--
--
module Sdma where

import Data.Word
import Text.Parsec (Line)
import Data.Char (isDigit)

type LineNumber = Line

data SdmaOperand = Register Word8 | -- r0..r7
                   Number Int |

                   Symbol String |
                   UnaryOp String SdmaOperand |
                   BinaryOp String SdmaOperand SdmaOperand |

                   Indexed Word8 SdmaOperand |
                   LabelRef Int LabelRefDirection |
                   Empty
                   deriving (Show, Eq)

data LabelRefDirection = Forward | Backward
    deriving (Show, Eq)

data SdmaInstruction = SdmaInstruction {
  siNmemonic :: String,
  siOperand0 :: SdmaOperand,
  siOperand1 :: SdmaOperand
  }
  deriving (Eq, Show)


symbolToRegister :: SdmaOperand -> SdmaOperand
symbolToRegister source@(Symbol s) =
  if not (length s == 2 && (head s) `elem` "rR" && (isDigit . head. tail) s )
  then
    source
  else
    Register (read (tail s) :: Word8)

symbolToRegister x = x


--
-- Local Variables:
-- coding: utf-8
-- End:
