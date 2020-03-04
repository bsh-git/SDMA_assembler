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
import Data.Set (singleton)
import Text.Printf
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
generate :: Word16 -> [LabelDef] -> [AsmLine] -> Parser [Word16]
generate _ _ [] = return []
generate pos ld ((AsmLine _ Nothing):xs) = generate pos ld xs
generate pos ld ((AsmLine _ (Just inst)):xs) = do
    c <- generateOne ld pos inst
    w <- generate (pos + 1) ld xs
    return (c:w)

type Generator = Parser Word16

generateOne :: [LabelDef] -> Word16 -> Statement -> Generator
generateOne ld pos statement@(Statement (WithPos _ nmemonicOffset nmemonic) _) = generateOne' statement
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
        "cpshreg" -> \_ -> return 0x06e2
        "done" -> special 0x0000 7
        "illegal" -> \_ -> return 0x0707
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
        "ret" -> \_ -> return 0x0006
        "revb" -> onereg 0x0010
        "revblo" -> onereg 0x0011
        "ror1" -> onereg 0x0014
        "rorb" -> onereg 0x0012
        "softbkpt" -> \_ -> return 0x0005
        "st" -> ldst 0x5800
        "stf" -> immediate 0x6800 0xff
        "sub" -> tworegs 0x00a0
        "subi" -> immediate 0x2000 0xff
        "tst" -> tworegs 0x00c0
        "tsti" -> immediate 0x4000 0xff
        "xor" -> tworegs 0x0090
        "xori" -> immediate 0x1000 0xff
        "yeild" -> \_ -> return 0x0000        -- done 0
        "yeildge" -> \_ -> return 0x0100       -- done 1

        ".dc" -> putData
        ".dc.w" -> putData

        _ -> \_ -> genError nmemonicOffset ("Unknown opcode " ++ nmemonic)


    onereg :: Word16 -> Statement -> Generator
    onereg pat = requireOperands 1 (onereg' pat)
    onereg' pat (Statement _ [opr]) = getRegNumber opr >>= (\r -> return (pat .|. (shift r 8)))
    onereg' _ _ = cantHappen

    tworegs :: Word16 -> Statement -> Generator
    tworegs pat = requireOperands 2 (tworegs' pat)
    tworegs' pat (Statement nm [l,r]) = tworegs'' pat nm l r
    tworegs' _ _ = cantHappen

    tworegs'' pat _ left right = do
        dest <- getRegNumber left
        src <- getRegNumber right
        return $ (shift dest 8) .|. pat .|. src

    immediate pat maxOpr = requireOperands 2 immediate'
      where
        immediate'  (Statement _ [reg, imm]) = do
            r <- getRegNumber reg
            i <- calcValue ld pos imm
            if i > maxOpr then genError (findOffset imm)
                                         $ "Immediate value out of range: " ++ (show i)
                           else return $ pat .|. (shift r 8) .|. (toWord16 i)
        immediate' _ = cantHappen

    branch pat = requireOperands 1 (branch' pat)
    branch' pat (Statement _ [opr]) = genBranch pat opr
    branch' _ _ = cantHappen

    genBranch :: Word16 -> AsmExpr -> Parser  Word16
    genBranch pat opr = do
        target <- calcExpr ld pos opr
        case target of
          (Num _) -> genError (findOffset opr)
                               $ "Bad operand for " ++ nmemonic
          (Address a) -> checkBranchTarget (-0x80) 0x7f pos a >>= (\disp ->
                return $ pat .|. (0xff .&. (toWord16 disp)) )


    jump :: Word16 -> Statement -> Generator
    jump pat = requireOperands 1 (genJump ld pos pat)

    ldst pat = requireOperands 2 ldst'
      where
        ldst' (Statement _ [reg, Indexed base disp]) = do
            r <- getRegNumber reg
            b <- getRegNumber base
            d <- calcValue ld pos disp
            if d < 0 || d >= 32
                then genError (findOffset disp)
                               $ "displacement overflow (" ++ (show d) ++ ")"
                else return $ pat .|. (shift r 8) .|. (shift (toWord16 d) 3) .|. b
        ldst' (Statement _ [_,expr]) =
            genError (findOffset expr)
                      $ "2nd operand of " ++ nmemonic ++ " must be a (Rn, disp)"
        ldst' _ = cantHappen


    loop :: Statement -> Generator
    loop (Statement nm []) = missing 1 nm
    loop (Statement _ [opr]) = loop' opr noExpr 0
    loop (Statement _ [opr,ff]) = calcValue ld pos ff >>= loop' opr ff
    loop s = genError (findOffset $ head $ stOperands s)
                       $ "too many operands. loop accepts 1 or 2 operands"

    loop' :: AsmExpr -> AsmExpr -> Int -> Generator
    loop' opr ffExpr ffVal = checkValueRange 0 3 ffVal ffExpr >> loop''
      where
        loop'' = calcExpr ld pos opr >>= loop'''
        loop''' (Address a) = checkBranchTarget 0 0xff pos a >>= loop_
        loop''' (Num v) = checkValueRange 1 0xff v opr >>= loop_
        loop_ lpsz = return $ 0x7800 .|. (shift (toWord16 ffVal) 8) .|. (toWord16 lpsz)

    noExpr = (error "can't happen" :: AsmExpr)

    special :: Word16 -> Int -> Statement -> Generator
    special pat maxOpr = requireOperands 1 special'
      where
        special' (Statement _ [opr]) = (calcValue ld pos opr) >>= special'' opr
        special' _ = cantHappen
        special'' flagsExpr flagsVal = checkValueRange 0 maxOpr flagsVal flagsExpr >>= (\v ->
            return $ (shift (toWord16 v) 8) .|. pat)

    putData :: Statement -> Generator
    putData (Statement nm []) = missing 1 nm
    putData (Statement _ [d]) = putData' d
    putData (Statement (WithPos _ off _) _) = genError off $ "not yet (more than one argument to .dc)"
    putData' :: AsmExpr -> Generator
    putData' d = calcExpr ld pos d >>= putData''
    putData'' (Address a) = return a
    putData'' (Num a) = return (fromIntegral a)


    checkValueRange mn mx val expr =
        if mn <= val && val <= mx
        then return (toWord16 val)
        else genError (findOffset expr)
                       $ "Value out of range for " ++ nmemonic ++ ": " ++ show (val)

    checkBranchTarget mn mx curPos target =
        let diff = (fromIntegral target::Int) - ((fromIntegral curPos::Int) + 1)
        in if mn <= diff && diff <= mx
        then return diff
        else do
          _ <- genError nmemonicOffset
                         $ "branch target out of range for " ++ nmemonic ++ ": "
                           ++ (printf "target=%#x .=%#x" target curPos)

          return 0

    registerErrorMessage = "a General Purpose register is required"
    getRegNumber (Register (WithPos _ off r)) =
        if r <= 7
        then return (toWord16 r)
        else genError off registerErrorMessage
    getRegNumber opr = genError (findOffset opr) registerErrorMessage



    requireOperands n f s@(Statement nm opr)
        | length opr > n = toomany n nm
        | length opr < n = missing n nm
        | otherwise = f s

    missing = badNumberOfOperands "missing an operand"
    toomany = badNumberOfOperands "too many operands"
    badNumberOfOperands s n (WithPos _ off nm) =
        genError off $ s ++ " for " ++ nm ++ ". requires " ++ (show n) ++ " operands."

genJump :: [LabelDef] -> Word16 -> Word16 -> Statement -> Generator
genJump ld pos pat (Statement _ [opr]) = (jump =<< (calcExpr ld pos opr))
  where
    jump (Address target) = jump' target   -- XXX need relocate
    jump (Num target) = jump' target -- jmp 0xDEAD
    jump' target =
      if target < 0 || target > 0x3fff
      then genError (findOffset opr)
                     $ "Address out of range: " ++ (printf "target=%#x" target)
      else return $ pat .|. (toWord16 target)
genJump _ _ _ _ = cantHappen


toWord16 :: (Integral a) => a -> Word16
toWord16 w = fromIntegral w :: Word16

genError :: Int -> String -> Generator
genError off s = do
  registerParseError $ FancyError off $ Data.Set.singleton (ErrorFail s)
  return (0 :: Word16)

cantHappen :: Generator
cantHappen = error "Can't happen"


--
-- Local Variables:
-- coding: utf-8
-- End:
