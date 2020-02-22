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
                   deriving (Eq, Show)

data LabelRefDirection = Forward | Backward
    deriving (Eq, Show)

data SdmaInstruction = SdmaInstruction {
  siNmemonic :: String,
  siOperand0 :: SdmaOperand,
  siOperand1 :: SdmaOperand
  }
  deriving (Eq, Show)

data Label = Label String
           | LocalLabel Int
    deriving (Eq, Show)

type InstructionLine = ([Label], Maybe SdmaInstruction, LineNumber)

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
