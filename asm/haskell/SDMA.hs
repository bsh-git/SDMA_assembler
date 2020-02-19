--
--
module SDMA where

import Data.Word

data SDMAOperand = Register Word8 | -- r0..r7
                   Number Int |

                   Symbol String |
                   UnaryOp String SDMAOperand |
                   BinaryOp String SDMAOperand SDMAOperand |

                   Indexed Word8 SDMAOperand |
                   Empty
                   deriving (Show, Eq)

data SDMAInstruction = SDMAInstruction {
  siNmemonic :: String,
  siOperand0 :: SDMAOperand,
  siOperand1 :: SDMAOperand
  }
  deriving (Eq, Show)


--
-- Local Variables:
-- coding: utf-8
-- End:
