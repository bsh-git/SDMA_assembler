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
import Sdma.Parser hiding (AsmToken)
import Sdma.Eval

import Debug.Trace

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
generateOne ld pos statement@(Statement (WithPos _ nmemonic) _) = generateOne' statement
  where
    generateOne' =
      case nmemonic of
        "add" -> tworegs 0x0098
        "addi" -> immediate 0x1800 0xff
        "and" -> tworegs 0x00b8
        "andi" -> immediate 0x3800 0xff
        "andn" -> tworegs 0x00b0
        "andni" -> immediate 0x3000 0xff
        "asr1" -> onereg 0x0016
        "bclri" -> immediate 0x0020 0x1f
        "bdf" -> branch 0x7f00
        "bf" -> branch 0x7c00
        "bseti" -> immediate 0x0040 0x1f
        "bsf" -> branch 0x7e00
        "bt" -> branch 0x7d00
        "btsti" -> immediate 0x0060 0x1f
        "clrf" -> special 0x0007 3
        "cmpeq" -> tworegs 0x00c8
        "cmpeqi" -> immediate 0x4800 0xff
        "cmphs" -> tworegs 0x00d8
        "cmplt" -> tworegs 0x00d0
        "cpshreg" -> \_ -> Right 0x06e2
        "done" -> special 0x0000 7
        "illegal" -> \_ -> Right 0x0707
        "jmp" -> jump 0x8000
        "jmpr" -> onereg 0x0008
        "jsr" -> jump 0xc000
        "jsrr" -> onereg 0x0009
        "ld" -> ldst 0x5000
        "ldf" -> immediate 0x6000 0xff
        "ldi" -> immediate 0x0800 0xff
        "ldrpc" -> onereg 0x000a
        "loop" -> loop
        "lsl1" -> onereg 0x0017
        "lsr1" -> onereg 0x0015
        "mov" -> tworegs 0x0088
        "notify" -> special 0x0001 7
        "or" -> tworegs 0x00a8
        "ori" -> immediate 0x2800 0xff
        "ret" -> \_ -> Right 0x0006
        "revb" -> onereg 0x0010
        "revblo" -> onereg 0x0011
        "ror1" -> onereg 0x0014
        "rorb" -> onereg 0x0012
        "softbkpt" -> \_ -> Right 0x0005
        "st" -> ldst 0x5800
        "stf" -> immediate 0x6800 0xff
        "sub" -> tworegs 0x00a0
        "subi" -> immediate 0x2000 0xff
        "tst" -> tworegs 0x00c0
        "tsti" -> immediate 0x4000 0xff
        "xor" -> tworegs 0x0090
        "xori" -> immediate 0x1000 0xff
        "yeild" -> \_ -> Right 0x0000        -- done 0
        "yeildge" -> \_ -> Right 0x0100       -- done 1

        ".dc" -> putData
        ".dc.w" -> putData

        _ -> \_ -> Left ("Unknown opcode " ++ nmemonic)


    onereg :: Word16 -> Statement -> Either String Word16
    onereg pat = requireOperands 1 (onereg' pat)
    onereg' pat (Statement _ [opr]) = getRegNumber opr >>= (\r -> return (pat .|. (shift r 8)))
    onereg' _ _ = cantHappen

    tworegs :: Word16 -> Statement -> Either String Word16
    tworegs pat = requireOperands 2 (tworegs' pat)
    tworegs' pat (Statement nm [l,r]) = tworegs'' pat nm l r
    tworegs' _ _ = cantHappen

    tworegs'' pat _ left right = do
        dest <- getRegNumber left
        src <- getRegNumber right
        Right $ (shift dest 8) .|. pat .|. src

    immediate pat maxOpr = requireOperands 2 immediate'
      where
        immediate'  (Statement _ [reg, imm]) = do
            r <- getRegNumber reg
            i <- calcValue ld pos imm
            if i > maxOpr then Left $ "Immediate value out of range: " ++ (show i)
                           else Right $ pat .|. (shift r 8) .|. (toWord16 i)
        immediate' _ = cantHappen

    branch pat = requireOperands 1 (branch' pat)
    branch' pat (Statement _ [opr]) = genBranch pat opr
    branch' _ _ = cantHappen

    genBranch :: Word16 -> AsmExpr -> Either String Word16
    genBranch pat opr = do
        target <- calcExpr ld pos opr
        case target of
          (Num _) -> Left $ "Bad operand for " ++ nmemonic ++ ": " ++ (show opr)
          (Address a) -> checkBranchTarget (-0x80) 0x7f pos a >>= (\disp ->
                Right $ pat .|. (0xff .&. (toWord16 disp)) )


    jump :: Word16 -> Statement -> Either String Word16
    jump pat = requireOperands 1 jump'
      where
        jump' (Statement _ [opr]) = jump'' =<< (calcExpr ld pos opr)
        jump' _ = cantHappen
        jump'' (Address target) = jump''' target  -- XXX need relocate
        jump'' (Num target) = jump''' target -- XXX

        jump''' target =
            if target < 0 || target > 0x3fff
            then Left $ "Address out of range (" ++ show target ++ ") for " ++ nmemonic
            else Right $ pat .|. (toWord16 target)

    ldst pat = requireOperands 2 ldst'
      where
        ldst' (Statement _ [reg, Indexed base disp]) = do
            r <- getRegNumber reg
            b <- getRegNumber base
            d <- calcValue ld pos disp
            if d < 0 || d >= 32
                then Left $ "displacement overflow (" ++ (show d) ++ ")"
                else Right $ pat .|. (shift r 8) .|. (shift (toWord16 d) 3) .|. b
        ldst' (Statement _ [_,_]) =
            Left $ "2nd operand of " ++ nmemonic ++ " must be a (Rn, disp)"
        ldst' _ = cantHappen


    loop :: Statement -> Either String Word16
    loop (Statement nm []) = missing 1 nm
    loop (Statement _ [opr]) = loop' opr 0
    loop (Statement _ [opr,ff]) = calcValue ld pos ff >>= loop' opr
    loop _ = Left $ "too many operands. loop accepts 1 or 2 operands"
    loop' :: AsmExpr -> Int -> Either String Word16
    loop' opr ff = checkValueRange 0 3 ff >> loop''
      where
        loop'' = calcExpr ld pos opr >>= loop'''
        loop''' (Address a) = checkBranchTarget 0 0xff pos a >>= loop_
        loop''' (Num v) = checkValueRange 1 0xff v >>= loop_
        loop_ lpsz = return $ 0x7800 .|. (shift (toWord16 ff) 8) .|. (toWord16 lpsz)


    special :: Word16 -> Int -> Statement -> Either String Word16
    special pat maxOpr = requireOperands 1 special'
      where
        special' (Statement _ [opr]) = (calcValue ld pos opr) >>= special''
        special' _ = cantHappen
        special'' flags = checkValueRange 0 maxOpr flags >>= (\v ->
            return $ (shift (toWord16 v) 8) .|. pat)

    putData :: Statement -> Either String Word16
    putData (Statement nm []) = missing 1 nm
    putData (Statement _ [d]) = putData' d
    putData (Statement _ _) = Left $ "not yet (more than one argument to .dc)"
    putData' :: AsmExpr -> Either String Word16
    putData' d = calcExpr ld pos d >>= putData''
    putData'' (Address a) = Right a
    putData'' (Num a) = Right (fromIntegral a)


    checkValueRange mn mx val =
        if mn <= val && val <= mx
        then Right val
        else Left $ "Value out of range for " ++ nmemonic ++ ": " ++ show (val)

    checkBranchTarget mn mx curPos target =
        let diff = (fromIntegral target::Int) - ((fromIntegral curPos::Int) + 1)
        in if mn <= diff && diff <= mx
        then return diff
        else Left $ "branch target out of range for " ++ nmemonic ++ ": " ++ show (target, curPos, diff)

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



--
-- Local Variables:
-- coding: utf-8
-- End:
