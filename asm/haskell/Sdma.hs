--
--
module Sdma where

import Data.Word

data SdmaOperand = Register Word8 | -- r0..r7
                   Number Int |

                   Symbol String |
                   UnaryOp String SdmaOperand |
                   BinaryOp String SdmaOperand SdmaOperand |

                   Indexed Word8 SdmaOperand |
                   Empty
                   deriving (Show, Eq)

data SdmaInstruction = SdmaInstruction {
  siNmemonic :: String,
  siOperand0 :: SdmaOperand,
  siOperand1 :: SdmaOperand
  }
  deriving (Eq, Show)


--
-- Local Variables:
-- coding: utf-8
-- End:
