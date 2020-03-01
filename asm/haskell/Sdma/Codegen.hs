--
--
module Sdma.Codegen
  ( LabelDef
  , fixLabels
  , generate
  ) where

import Data.Word
import Data.Bits
import Data.Maybe
import Data.List (foldl')
import Sdma.Parser

import Debug.Trace

type LabelDef = (Word16, [WithPos Label])

--
-- pass1
--
fixLabels :: Word16 -> [AsmLine] -> [LabelDef]
fixLabels start = snd . (foldl' step (start, []))
    where step (pos, ld) (AsmLine lbls insn) =
              let newpos = pos + if isJust insn then 1 else 0
                  newld = addLabelDef ld lbls pos
              in
                (newpos, newld)

--
-- returns
--   Left "error message"
--   Right (binaries, label definitions)
--
generate :: Word16 -> [LabelDef] -> [AsmLine] -> Either String [Word16]
generate _ _ [] = Right []
generate pos ld ((AsmLine _ Nothing):xs) = generate pos ld xs
generate pos ld ((AsmLine _ (Just inst)):xs) = do
    c <- generateOne ld pos inst
    w <- generate (pos + 1) ld xs
    return (c:w)

generateOne :: [LabelDef] -> Word16 -> Statement -> Either String Word16
generateOne ld pos statement@(Statement (WithPos _ nmemonic) _) =
    case nmemonic of
      "add" -> tworegs 0x00 0x98 statement 
      "addi" -> immediate 0x1800 0xff statement 
      "and" -> tworegs 0x00 0xb8 statement 
      "andi" -> immediate 0x3800 0xff statement 
      "andn" -> tworegs 0x00 0xb0 statement 
      "andni" -> immediate 0x3000 0xff statement 
      "asr1" -> onereg 0x0016 statement 
      "bclri" -> immediate 0x0020 0x1f statement 
      "bdf" -> branch 0x7f00 statement
      "bf" -> branch 0x7c00 statement
      "bseti" -> immediate 0x0040 0x1f statement
      "bsf" -> branch 0x7e00 statement
      "bt" -> branch 0x7d00 statement
      "btsti" -> immediate 0x0060 0x1f statement
      "clrf" -> clrf statement
      _ -> Left ("Unknown opcode " ++ nmemonic)

  where
    onereg :: Word16 -> Statement -> Either String Word16
    onereg pat = requireOperands 1 (onereg' pat)
    onereg' pat (Statement _ [opr]) = getRegNumber opr >>= (\r -> return (pat .|. (shift r 8)))
    onereg' _ _ = cantHappen

    tworegs :: Word16 -> Word16 -> Statement -> Either String Word16
    tworegs pat1 pat2 = requireOperands 2 (tworegs' pat1 pat2)
    tworegs' pat1 pat2 (Statement nm [l,r]) = tworegs'' pat1 pat2 nm l r
    tworegs' _ _ _ = cantHappen

    tworegs'' pat1 pat2 _ left right = do
        dest <- getRegNumber left
        src <- getRegNumber right
        Right $ (shift (pat1 .|. dest) 8) .|. pat2 .|. src

    immediate w maximam = requireOperands 2 (immediate' w maximam)
    immediate' w maximam (Statement _ [reg, imm]) = do
        r <- getRegNumber reg
        i <- calcExpr imm
        if i > maximam then Left $ "Immediate value out of range: " ++ (show i)
                       else Right $ w .|. (shift r 8) .|. (toWord16 i)
    immediate' _ _ _ = cantHappen

    branch pat = requireOperands 1 (branch' pat)
    branch' pat (Statement nm [opr]) = genBranch pat ld nm opr pos
    branch' _ _ = cantHappen

    clrf = requireOperands 1 clrf'
    clrf' (Statement _ [opr]) = clrf'' =<< (calcExpr opr)
      where
        clrf'' ff = if ff < 0 || ff > 3
                  then Left $ "Bad value " ++ show ff ++ " for clrf"
                  else Right $ (shift 8 ff) .|. 0x0007
    clrf' _ = cantHappen

    getRegNumber s@(Register (WithPos _ r)) =
        if r <= 7
        then Right (toWord16 r)
        else Left $ "Bad register: " ++ (show s)
    getRegNumber opr = Left $ "Bad register " ++ (show opr)

    toWord16 w8 = fromIntegral w8 :: Word16


    requireOperands n f s@(Statement nm opr)
        | length opr > n = toomany n nm
        | length opr < n = missing n nm
        | otherwise = f s

    missing = badNumberOfOperands "missing an operand"
    toomany = badNumberOfOperands "too many operands"
    badNumberOfOperands s n (WithPos _ nm) =
        fail $ s ++ "for " ++ nm ++ ". requires " ++ (show n) ++ " operands."

    cantHappen = error "Can't happen"

--addLabelDefDebug ld new pos = traceShow (ld, new, pos) $ addLabelDef ld new pos

addLabelDef :: [LabelDef] -> [WithPos Label] -> Word16 -> [LabelDef]
addLabelDef ld [] _ = ld
addLabelDef [] new pos = [(pos, new)]
addLabelDef ld@((pos0, lbl):ls) new pos
    | pos0 == pos = (pos, new ++ lbl):ls   -- multiple labels at the same position in several lines
    | otherwise = (pos, new):ld
                     

genBranch :: Word16 -> [LabelDef] -> (WithPos String) -> AsmExpr -> Word16 -> Either String Word16
genBranch = undefined
{-
genBranch w labelDef (WithPos srcPos nmemonic) opr pos = do
    target <- calcExpr opr
    getBranchTarget labelDef (fromIntegral target) pos >>= gen
  where
    gen = let disp = (fromIntegral target) - (fromIntegral (pos +1)) :: Int
                 in
                   if disp > 0x7f || (-0x80) > disp
                   then Left $ "Branch target too far " ++ show (siOperand0 insn)
                   else Right $ w .|. (0xff .&. ((fromIntegral disp) :: Word16))
-}

getBranchTarget :: [LabelDef] -> Word16 -> Word16 -> Either String Word16
{-
getBranchTarget _ (Number d) _ = Right (fromIntegral d :: Word16)
getBranchTarget ld (Symbol s) _ = findLabel ld s
getBranchTarget ld (LabelRef n dir) pos = findLocalLabel ld n dir pos
getBranchTarget _ x _ = Left $ "Not supported yet: branch target: " ++ show x
-}
getBranchTarget = undefined

findLabel :: [LabelDef] -> String -> Either String Word16
findLabel ld s = case found of
                   [] -> Left $ "Label " ++ s ++ " not found"
                   [x] -> Right $ fst x
                   _ -> Left $ "Duplicate definition of " ++ s
    where found = filter isLabelDefined ld
          isLabelDefined d = (Label s) `elem` map tokenVal (snd d)

-- (Label s) `elem` (snd d)

findLocalLabel :: [LabelDef] -> Int -> LabelRefDirection -> Word16 -> Either String Word16
findLocalLabel ld n dir pos =
    case (null filtered, dir) of
     (True,_) -> Left $ "Local label " ++ (show n) ++ " not defined"
     (False,Forward) -> Right $ fst $ last filtered
     (False,Backward) -> Right $ fst $ head filtered
  where
    match d = (LocalLabel n) `elem` map tokenVal (snd d)
    dirOk = if dir == Forward then forward else backward
    forward d = (fst d) > pos
    backward d = (fst d) <= pos
    filtered = filter (\d -> (match d && dirOk d)) ld


calcExpr :: AsmExpr -> Either String Int
calcExpr expr =
    case expr of
      Leaf (WithPos _ (Number n)) -> Right (fromIntegral n)
      _ -> Left $ "Not supported yet" ++ (show expr)

--
-- Local Variables:
-- coding: utf-8
-- End:
