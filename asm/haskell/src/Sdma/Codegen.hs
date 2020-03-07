{-# LANGUAGE TupleSections #-}
--
--
module Sdma.Codegen
  ( LabelDef
  , CodeAddr(..)
  , fixLabels
  , generate
  , module Sdma.Base
  ) where

import Data.Word
import Data.Bits
import Data.List.Singleton
import Data.Foldable
import qualified Data.Set (singleton)
import Text.Printf
import Sdma.Parser hiding (AsmToken)
import Sdma.Base
import Sdma.Eval
-- import Text.Megaparsec.Debug

--import Debug.Trace

--
-- pass1
-- set address to all labels
--
fixLabels :: CodeAddr -> [AsmLine] -> [LabelDef]
fixLabels start = snd . (foldl' step (start, []))
    where step (pos, ld) (AsmLine lbls insn) =
              let newpos = advance insn pos
                  newld = addLabelDef ld lbls (toWordAddr pos)
              in
                (newpos, newld)
          advance Nothing pos = pos
          advance (Just (Statement (WithPos _ nmemonic) opr)) pos =
              case nmemonic of
                ".dc.b" -> advanceAddr (toByteAddr pos) (length opr)
                ".dc.w" -> wordAdvance pos (length opr)
                ".dc" -> wordAdvance pos (length opr)
                ".dc.l" -> alignAddr 2 $ wordAdvance  pos $ 2 * length opr
                _ -> wordAdvance pos 1
          wordAdvance a l = advanceAddr (toWordAddr a) l


--
-- Machine code generated by generator
--   all instructions are one-word code.
--   directives such as .dc.w and .dc.l can generate multiple word, and
--   .dc.b and .ascii can generate multiple byte code
--
data MachineCode =
      Bytes { mcBytes :: [Word8] }
    | Words [Word16]
    deriving (Show)

isBytes :: MachineCode -> Bool
isBytes (Bytes _) = True
isBytes (Words _) = False

type GenInstruction = Parser Word16


--
--
generate :: CodeAddr -> [LabelDef] -> [AsmLine] -> Parser [Word16]
generate pos ld lns =
    generate' pos lns >>= return . concatCodes

  where
    generate' :: CodeAddr -> [AsmLine] -> Parser [MachineCode]
    generate' _ [] = return []
    generate' a ((AsmLine _ Nothing):xs) = generate' a xs
    generate' a ((AsmLine _ (Just inst)):xs) = do
        (next,c) <- generateOne ld a inst
        r <- generate' next xs
        return (c:r)

    concatCodes :: [MachineCode] -> [Word16]
    concatCodes = concat . concatBytes

    concatBytes [] = []
    concatBytes (Words w:xs) = w : concatBytes xs
    concatBytes cs = let (bs, rest) = span isBytes cs
                     in
                         (map bytesToWord $ pairs $ concat $ map mcBytes bs) : concatBytes rest
    pairs [] = []
    pairs [a] = [[a]]
    pairs (a:b:xs) = [a,b] : pairs xs
    bytesToWord [a] = bytesToWord [a,0]
    bytesToWord [a,b] = (shift (toWord16 a) 8) .|. (toWord16 b)
    bytesToWord _ = error "Can't happen"

generateOne :: [LabelDef] -> CodeAddr -> Statement -> Parser (CodeAddr, MachineCode)
generateOne ld pos statement@(Statement (WithPos nmemonicOffset nmemonic) opr) =
  -- directives that generate other than one-word code
    case nmemonic of
      --".ascii"   -- not yet
      --".asciz"   -- not yet
      ".dc.w" -> genWordData
      ".dc" -> genWordData
      ".dc.b" -> let a = toByteAddr pos
                 in
                   if null opr then missingOperands a
                   else do
                       b <- genData getByte a opr :: Parser [Word8]
                       return (advanceAddr a (length b), Bytes b)
      ".dc.l" -> genLongData
      _ -> let aligned = toWordAddr pos
               nextAddr = incrementAddr aligned
           in
             generateWord ld pos statement >>= (return . (nextAddr,) . Words . singleton)

  where
    errorMsg = "missing arguments for " ++ nmemonic
    missingOperands a = generatorError (a, Bytes []) nmemonicOffset errorMsg

    getData addressAllowed addr expr = do
        val <- calcExpr ld addr expr
        case (isAbsolute pos, addressAllowed, val) of
           (_, _, Num n) -> return n
           (True, True, Address a) -> return $ fromIntegral a
           (_, False, _) -> generatorError 0 (findOffset expr) "address not allowed"
           (False, _, _) -> generatorError 0 (findOffset expr) "relocatable address not allowed"

    toWord8 v = (fromIntegral v) :: Word8
    getByte a e = getData False a e >>= pure . toWord8 >>= checkValueRange 0 0xff e
    getWord a e = getData True a e >>= pure . toWord16 >>= checkValueRange 0 0xffff e
    toWord32 v = (fromIntegral v) :: Word32
    getLong a e = getData True a e >>= pure . toWord32
    genData :: Integral a => (CodeAddr -> AsmExpr -> Parser a)
                          -> CodeAddr -> [AsmExpr] -> Parser [a]
    genData getf addr0 o = foldlM genData' (addr0,[]) o >>= (return . snd)
       where
         genData' :: Integral a => (CodeAddr, [a]) -> AsmExpr -> Parser (CodeAddr, [a])
         genData' (addr, dts) o' = do
             dt <- getf addr o'
             return (advanceAddr addr 1, dts ++ [fromIntegral dt])

    genWordData = let a = toWordAddr pos
                  in
                    if null opr
                    then missingOperands a
                    else do
                             w <- genData getWord a opr
                             return (advanceAddr a (length w), Words (map fromIntegral w))

    genLongData = let a0 = toWordAddr pos
                      a = alignAddr 2 a0
                  in
                    if null opr
                    then missingOperands a
                    else do
                            ls <- genData getLong a opr
                            let ws = concat $ map (\l -> [ toWord16 ((shift l (-16)) .&. 0xffff)
                                                                 , toWord16 (l .&. 0xffff)]) ls
                            return ( advanceAddr a (2 * (length ls))
                                   , Words $ if a0 == a then ws else 0:ws )



generateWord :: [LabelDef] -> CodeAddr -> Statement -> GenInstruction
generateWord ld pos statement@(Statement (WithPos nmemonicOffset nmemonic) _) = generateOne' statement
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

        _ -> \_ -> genError nmemonicOffset ("Unknown opcode " ++ nmemonic)


    onereg :: Word16-> Statement -> GenInstruction
    onereg pat = requireOperands 1 (onereg' pat)
    onereg' pat (Statement _ [opr]) = getRegNumber opr >>= (\r -> return (pat .|. (shift r 8)))
    onereg' _ _ = cantHappen

    tworegs :: Word16 -> Statement -> GenInstruction
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


    jump :: Word16 -> Statement -> GenInstruction
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


    loop :: Statement -> GenInstruction
    loop (Statement nm []) = missing 1 nm
    loop (Statement _ [opr]) = loop' opr noExpr 0
    loop (Statement _ [opr,ff]) = calcValue ld pos ff >>= loop' opr ff
    loop s = genError (findOffset $ head $ stOperands s)
                       $ "too many operands. loop accepts 1 or 2 operands"

    loop' :: AsmExpr -> AsmExpr -> Int -> GenInstruction
    loop' opr ffExpr ffVal = checkValueRange 0 3 ffExpr ffVal >> loop''
      where
        loop'' = calcExpr ld pos opr >>= loop'''
        loop''' (Address a) = checkBranchTarget 0 0xff pos a >>= loop_
        loop''' (Num v) = checkValueRange 1 0xff opr v >>= loop_
        loop_ lpsz = return $ 0x7800 .|. (shift (toWord16 ffVal) 8) .|. (toWord16 lpsz)

    noExpr = (error "can't happen" :: AsmExpr)

    special :: Word16 -> Int -> Statement -> GenInstruction
    special pat maxOpr = requireOperands 1 special'
      where
        special' (Statement _ [opr]) = (calcValue ld pos opr) >>= special'' opr
        special' _ = cantHappen
        special'' flagsExpr flagsVal = checkValueRange 0 maxOpr flagsExpr flagsVal >>= (\v ->
            return $ (shift (toWord16 v) 8) .|. pat)

    checkBranchTarget mn mx curPos target =
        let diff = (fromIntegral target::Int) - (fromIntegral (getAddr (incrementAddr curPos)))
        in if mn <= diff && diff <= mx
        then return diff
        else do
          _ <- genError nmemonicOffset
                         $ "branch target out of range for " ++ nmemonic ++ ": "
                           ++ (printf "target=%#x .=%#x" target (getAddr curPos))

          return 0

    registerErrorMessage = "a General Purpose register is required"
    getRegNumber (Register (WithPos off r)) =
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

badNumberOfOperands :: String -> Int -> (WithPos String) -> GenInstruction
badNumberOfOperands s n (WithPos off nm) =
    genError off $ s ++ " for " ++ nm ++ ". requires " ++ (show n) ++ " operands."

checkValueRange :: (Integral a, Show a) => Int -> Int -> AsmExpr -> a -> Parser a
checkValueRange mn mx expr val =
    if mn <= (fromIntegral val) && (fromIntegral val) <= mx
    then return val
    else generatorError val (findOffset expr)
         $ "Value out of range for: " ++ (show val)



genJump :: [LabelDef] -> CodeAddr -> Word16 -> Statement -> GenInstruction
genJump ld pos pat (Statement _ [opr]) = (jump =<< (calcExpr ld pos opr))
  where
    jump (Num target) = jump' target -- jmp 0xDEAD
    jump (Address target) =
        if isAbsolute pos  -- are we assembling to absolute address?
        then jump' target
        else genError off "relocatable address cannot be used. (enabled in the future)"
    jump' target =
      if target < 0 || target > 0x3fff
      then genError off
                     $ "Address out of range: " ++ (printf "target=%#x" target)
      else return $ pat .|. (toWord16 target)
    off = findOffset opr
genJump _ _ _ _ = cantHappen


toWord16 :: (Integral a) => a -> Word16
toWord16 w = fromIntegral w :: Word16

genError :: Int -> String -> GenInstruction
genError = generatorError (0 :: Word16)

generatorError :: a -> Int -> String -> Parser a
generatorError ret off s = do
  registerParseError $ FancyError off $ Data.Set.singleton (ErrorFail s)
  return ret

cantHappen :: GenInstruction
cantHappen = error "Can't happen"


--
-- Local Variables:
-- coding: utf-8
-- End:
