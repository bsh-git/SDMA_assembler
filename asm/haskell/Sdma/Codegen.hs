--
--
module Sdma.Codegen
  ( LabelDef
  , locateLabels
  , generate
  ) where

import Data.Word
import Data.Bits
import Data.Maybe
import Data.List (foldl')
import Sdma

import Debug.Trace

type LabelDef = (Word16, [Label])

--
-- pass1
--
locateLabels :: Word16 -> [InstructionLine] -> [LabelDef]
locateLabels start = snd . (foldl' step (start, []))
    where step (pos, ld) (lbls, insn, lno) =
              let newpos = pos + if isJust insn then 1 else 0
                  newld = addLabelDef ld lbls pos
              in
                (newpos, newld)

--
-- returns
--   Left "error message"
--   Right (binaries, label definitions)
--
generate :: Word16 -> [LabelDef] -> [InstructionLine] -> Either String [Word16]
generate _ ld [] = Right []
generate pos ld (x@(_, inst, lno):xs) =
    if isNothing inst
    then generate pos ld xs
    else do
        c <- generateOne x ld pos
        w <- generate (pos + 1) ld xs
        return (c:w)

generateOne :: InstructionLine -> [LabelDef] -> Word16 -> Either String Word16
generateOne (_, Nothing, _) _ _ = undefined
generateOne (_, Just instruction, lno) ld pos =
    case siNmemonic instruction of
      "add" -> tworegs 0x00 0x98 instruction 
      "addi" -> immediate 0x1800 0xff instruction 
      "and" -> tworegs 0x00 0xb8 instruction 
      "andi" -> immediate 0x3800 0xff instruction 
      "andn" -> tworegs 0x00 0xb0 instruction 
      "andni" -> immediate 0x3000 0xff instruction 
      "asr1" -> onereg 0x0016 instruction 
      "bclri" -> immediate 0x0020 0x1f instruction 
      "bdf" -> branch 0x7f00
      "bf" -> branch 0x7c00
      "bseti" -> immediate 0x0040 0x1f instruction
      "bsf" -> branch 0x7e00
      "bt" -> branch 0x7d00
      "btsti" -> immediate 0x0060 0x1f instruction
      "clrf" -> genClrf (siOperand0 instruction)
      _ -> Left ("Unknown opcode " ++ (siNmemonic instruction))

  where
    onereg :: Word16 -> SdmaInstruction -> Either String Word16
    onereg n ins = getRegNumber (siOperand0 ins) >>= (\r -> return (n .|. (shift r 8)))
    tworegs :: Word16 -> Word16 -> SdmaInstruction -> Either String Word16
    tworegs n b ins = getRegs ins >>= tworegs' n b
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

    immediate w maximm ins = do
        r <- getRegNumber (siOperand0 ins)
        i <- getImm (siOperand1 ins)
        if i > maximm then Left $ "Immediate value out of range: " ++ (show i)
                      else Right $ w .|. (shift r 8) .|. (toWord16 i)
    getImm (Number n) = Right n
    branch w = genBranch w ld instruction pos


--addLabelDefDebug ld new pos = traceShow (ld, new, pos) $ addLabelDef ld new pos

addLabelDef :: [LabelDef] -> [Label] -> Word16 -> [LabelDef]
addLabelDef ld [] _ = ld
addLabelDef [] new pos = [(pos, new)]
addLabelDef ld@((pos0, lbl):ls) new pos
    | pos0 == pos = (pos, new ++ lbl):ls   -- multiple labels at the same position in several lines
    | otherwise = (pos, new):ld
                     

genBranch :: Word16 -> [LabelDef] -> SdmaInstruction -> Word16 -> Either String Word16
genBranch w labelDef insn pos =
    either (\m -> Left m) gen $ getBranchTarget labelDef (siOperand0 insn) pos
  where
    gen target = let disp = (fromIntegral target) - (fromIntegral (pos +1)) :: Int
                 in
                   if disp > 0x7f || (-0x80) > disp
                   then Left $ "Branch target too far " ++ show (siOperand0 insn)
                   else Right $ w .|. (0xff .&. ((fromIntegral disp) :: Word16))

getBranchTarget :: [LabelDef] -> SdmaOperand -> Word16 -> Either String Word16
getBranchTarget _ (Number d) _ = Right (fromIntegral d :: Word16)
getBranchTarget ld (Symbol s) _ = findLabel ld s
getBranchTarget ld (LabelRef n dir) pos = findLocalLabel ld n dir pos
getBranchTarget _ x _ = Left $ "Not supported yet: branch target: " ++ show x

findLabel :: [LabelDef] -> String -> Either String Word16
findLabel ld s = case found of
                   [] -> Left $ "Label " ++ s ++ " not found"
                   [x] -> Right $ fst x
                   _ -> Left $ "Duplicate definition of " ++ s
    where found = filter (\d -> (Label s) `elem` (snd d)) ld

findLocalLabel :: [LabelDef] -> Int -> LabelRefDirection -> Word16 -> Either String Word16
findLocalLabel ld n dir pos =
    case (null filtered, dir) of
     (True,_) -> Left $ "Local label " ++ (show n) ++ " not defined"
     (False,Forward) -> Right $ fst $ last filtered
     (False,Backward) -> Right $ fst $ head filtered
  where
    match d = (LocalLabel n) `elem` (snd d)
    dirOk = if dir == Forward then forward else backward
    forward d = (fst d) > pos
    backward d = (fst d) <= pos
    filtered = filter (\d -> (match d && dirOk d)) ld

genClrf :: SdmaOperand -> Either String Word16
genClrf opr = case opr of
  (Number f) ->  if f < 0 || f > 3
                 then Left $ "Bad value " ++ show f ++ " for clrf"
                 else Right $ (shift 8 f) .|. 0x0007
  _ -> Left "Not supported yet"

--
-- Local Variables:
-- coding: utf-8
-- End:
