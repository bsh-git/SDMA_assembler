--
--
module Sdma.Codegen
  ( LabelDef
  , generate
  ) where

import Data.Word
import Data.Bits
import Data.Maybe
import Sdma

import Debug.Trace

type LabelDef = (Word16, [String])

--
-- returns
--   Left "error message"
--   Right (binaries, label definitions)
--
generate :: Word16 -> [([String], Maybe SdmaInstruction, LineNumber)] -> Either String ([LabelDef], [Word16])
generate start = generate' start []
  where generate' _ ld [] = Right (ld, [])
        generate' pos ld (x@(lbls, inst, lno):xs) =
            let newld = if (not . null) lbls then addLabelDef ld lbls pos
                                             else ld
            in
              if isNothing inst
              then generate' pos newld xs
              else do
                  c <- generateOne x
                  (l, w) <- generate' (pos + 1) newld xs
                  return (l, (c:w))

generateOne :: ([String], Maybe SdmaInstruction, LineNumber) -> Either String Word16
generateOne (_, Nothing, _) = undefined
generateOne (_, Just instruction, lno) =
    case siNmemonic instruction of
      "add" -> tworegs instruction 0x00 0x98
      "addi" -> immediate instruction 0x1800 0xff
      "and" -> tworegs instruction 0x00 0xb8
      "andi" -> immediate instruction 0x3800 0xff
      "andn" -> tworegs instruction 0x00 0xb0
      "andni" -> immediate instruction 0x3000 0xff
      "asr1" -> onereg instruction 0x0016
      "bclri" -> immediate instruction 0x0020 0x1f
      _ -> Left ("Unknown opcode " ++ (siNmemonic instruction))

  where
    onereg :: SdmaInstruction -> Word16 -> Either String Word16
    onereg ins n = getRegNumber (siOperand0 ins) >>= (\r -> return (n .|. (shift r 8)))
    tworegs :: SdmaInstruction -> Word16 -> Word16 -> Either String Word16
    tworegs ins n b = getRegs ins >>= tworegs' n b
    tworegs' n b (dest, src) = Right $ (shift (n .|. dest) 8) .|. b .|. src
    getRegs ins = do
        dest <- getRegNumber (siOperand0 ins)
        src <- getRegNumber (siOperand1 ins)
        return (dest, src)
    getRegNumber (Register r) = Right (toWord16 r)
    getRegNumber s@(Symbol ss) = case symbolToRegister s of
                                   (Register r) -> if r <= 7 then Right (toWord16 r)
                                                             else Left $ "Bad register: " ++ ss
                                   _ -> Left $ "Bad register: " ++ ss
    getRegNumber opr = Left (show opr)
    toWord16 w8 = fromIntegral w8 :: Word16

    immediate ins w maximm = do
        r <- getRegNumber (siOperand0 ins)
        i <- getImm (siOperand1 ins)
        if i > maximm then Left $ "Immediate value out of range: " ++ (show i)
                      else Right $ w .|. (shift r 8) .|. (toWord16 i)
    getImm (Number n) = Right n


addLabelDef :: [LabelDef] -> [String] -> Word16 -> [LabelDef]
addLabelDef [] new pos = [(pos, new)]
addLabelDef ld@((pos0, lbl):ls) new pos
    | pos0 == pos = (pos, new ++ lbl):ls   -- multiple labels at the same position in several lines
    | otherwise = (pos, new):ld
                     


--
-- Local Variables:
-- coding: utf-8
-- End:
