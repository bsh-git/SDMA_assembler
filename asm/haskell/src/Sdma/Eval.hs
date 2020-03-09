--
--
module Sdma.Eval where

import Data.Word
import Data.Set (singleton)
import Data.Bits
import Control.Monad.Writer
import Text.Printf
import Sdma.Parser
import Sdma.Base
import Debug.Trace

type LabelDef = (Word16, [WithPos Label])

addLabelDef :: [LabelDef] -> [WithPos Label] -> CodeAddr -> [LabelDef]
addLabelDef ld [] _ = ld
addLabelDef _ _ (ByteAddr _ _) = error "byte address is not allowed for label"
addLabelDef [] new (WordAddr _ a) = [(a, new)]
addLabelDef ld@((pos0, lbl):ls) new (WordAddr _ pos)
    | pos0 == pos = (pos, new ++ lbl):ls   -- multiple labels at the same position in several lines
    | otherwise = (pos, new):ld


findLabel :: [LabelDef] -> String -> Maybe Word16
findLabel ld s = case found of
                   [] -> Nothing
                   [x] -> Just $ fst x
                   _ -> Nothing
    where found = filter isLabelDefined ld
          isLabelDefined d = (Label s) `elem` map tokenVal (snd d)

findLocalLabel :: [LabelDef] -> Word -> LabelRefDirection -> CodeAddr -> Maybe Word16
findLocalLabel _ _ _ (ByteAddr _ _) = error "byte address is not allowed"
findLocalLabel ld n dir (WordAddr _ a) = findLocalLabel' a
  where
    findLocalLabel' pos =
        case (null filtered, dir) of
          (True,_) -> Nothing
          (False,Forward) -> Just $ fst $ last filtered
          (False,Backward) -> Just $ fst $ head filtered
      where
        match d = (LocalLabel n) `elem` map tokenVal (snd d)
        dirOk = if dir == Forward then forward else backward
        forward d = (fst d) > pos
        backward d = (fst d) <= pos
        filtered = filter (\d -> (match d && dirOk d)) ld

--
-- result of evaluating expression
--
data Value = Num Int
           | Address Word16
           | Bad  -- error found. error message has already registered.
           deriving (Eq, Show)

calcExpr :: Maybe [LabelDef] -- ^ defined labels. if Nothing, labels are not allowed
         -> CodeAddr         -- ^ location of the code
         -> AsmExpr          -- ^ expression to evaluate
         -> Parser Value     -- ^ result of evaluation
calcExpr labelDef a expr =
    let (val, errors) = runWriter $ calcExpr' labelDef a expr
        register (WithPos off msg) =
            registerParseError $ FancyError off $ Data.Set.singleton (ErrorFail msg)
    in
      mapM_ register errors >> return val

type Evaluator = Writer [WithPos String] Value

calcExpr' :: Maybe [LabelDef] -> CodeAddr -> AsmExpr -> Evaluator
calcExpr' labelDef ba@(ByteAddr _ _) expr = calcExpr' labelDef (toWordAddr ba) expr
calcExpr' labelDef wa@(WordAddr _ pos) expr =
    case expr of
      Leaf (WithPos _ (Number n)) -> return (Num (fromIntegral n))
      Leaf (WithPos off (Identifier s)) ->
          if s == sToBs "."
          then return (Address pos)  -- `.' for current address
          else ifLabelAllowed off (\ld ->
            case findLabel ld (bsToS s) of
              Nothing -> evalError off $ printf "%s: not defined" (bsToS s)
              Just a -> return (Address a))

      Leaf (WithPos off (LocalLabelRef dir n)) ->
          ifLabelAllowed off (\ld ->
            case findLocalLabel ld n dir wa of
              Nothing -> evalError off $ printf "local label %d: not found" n
              Just a -> return (Address a))

      Leaf (WithPos off t) -> evalError off $ printf "Invalid token %s" (show t)

      UnaryExpr op e -> unaryExpr labelDef wa  op e
      BinaryExpr op left right -> binaryExpr labelDef wa op left right

      (Indexed _ _) -> evalError (findOffset expr) "Invalid expression"
      (Register _) -> evalError (findOffset expr) "Invalid expression"

  where
    ifLabelAllowed off whenAllowed =
        case labelDef of
          Nothing -> evalError off "labels are not allowed"
          Just ld -> whenAllowed ld


--
-- acceptable expressions involving address are
--  ADDRESS + NUM
--  ADDRESS - NUM
--  ADDRESS - ADDRESS
--  (- ADDRESS) + ADDRESS
--  followings may be useful, but not supported for now.
--  ADDRESS >> NUM
--  ADDRESS << NUM
--  ADDRESS & NUM

unaryExpr :: Maybe [LabelDef] -> CodeAddr -> (WithPos String) -> AsmExpr -> Evaluator
unaryExpr labelDef wa (WithPos opOff op) expr = do
    expr' <- calcExpr' labelDef wa expr
    case (op, expr') of
      (_, Bad) -> return Bad
      ("+", _) -> return $ expr'
      ("-", (Num v)) -> return (Num (-v))
      ("~", (Num v)) -> return (Num (fromIntegral (complement (fromIntegral v)::Word32)))
      (_, _) -> evalError opOff $ printf "Unkonw unary operator %s" op

binaryExpr :: Maybe [LabelDef] -> CodeAddr -> (WithPos String) -> AsmExpr -> AsmExpr -> Evaluator
binaryExpr labelDef wa opw@(WithPos _ op) left right = do
    -- convert (-a)+b to b-a
    case (op, left, right) of
      ("+", UnaryExpr uop@(WithPos _ "-") e, r) -> binaryExpr labelDef wa uop r e
      _ -> binaryExpr' labelDef wa opw left right


binaryExpr' :: Maybe [LabelDef] -> CodeAddr -> (WithPos String) -> AsmExpr -> AsmExpr -> Evaluator
binaryExpr' labelDef wa (WithPos opOff op) left right = do
    left' <- calcExpr' labelDef wa left
    right' <- calcExpr' labelDef wa right
    case (left', right') of
      (Bad, _) -> return Bad
      (_, Bad) -> return Bad
      ((Address _), (Address _)) ->
          if op /= "-"
          then evalError opOff (printf "Bad expression: ADDRESS  %s ADDRSS" op)
          else binaryArith left' right'
      (_, _) -> binaryExpr'' left' right'

  where
    binaryExpr'' = if isArithmeticOp then binaryArith else binaryBits
    isArithmeticOp = op `elem` ["+", "-", "*", "/", "%"]
    binaryArith l r =
        case (op, l, r) of
          ("+", (Num l'), (Num r')) -> return $ Num (l' + r')
          ("+", (Address addr), (Num v)) -> return $ Address (addr + (fromIntegral v))
          ("+", (Num v), (Address addr)) -> return $ Address (addr + (fromIntegral v))
          ("-", (Address l'), (Address r')) -> return $ Num $ fromIntegral l' - fromIntegral r'
          ("-", (Address addr), (Num v)) -> return $ Address (addr - (fromIntegral v))
          ("-", (Num v), (Address addr)) -> return $ Address ((fromIntegral v)- addr)
          ("-", (Num l'), (Num r')) -> return $ Num (l' - r')
          (_, (Address _), _) -> evalError opOff $ printf "Bad expression: ADDRESS %s ADDRSS" op
          ("*", (Num l'), (Num r')) -> return $ Num (l' * r')
          ("/", (Num l'), (Num r')) -> return $ Num (l' `div` r')
          ("%", (Num l'), (Num r')) -> return $ Num (l' `mod` r')
          (_, _, _) -> evalError opOff $ printf "Unsupported expression: %s" (show (op, left, right))

    binaryBits l r =
        case (op, l, r) of
          (">>", _, (Address _)) -> badShift
          ("<<", _, (Address _)) -> badShift
          ("<<", (Num v), (Num sft)) -> return $ Num $ fromIntegral (shift (fromIntegral v::Word32) sft)
          (">>", (Num v), (Num sft)) -> return $ Num $ fromIntegral (shift (fromIntegral v::Word32) (- sft))
          (_, (Num _), (Num _)) -> (return . Num . fromIntegral) (binaryBits' (toW32 l) (toW32 r))
          (_, (Address _), (Num _)) -> notSupported
          (_, _, _) -> cantHappen

    toW32 (Num v) = (fromIntegral v) :: Word32
    toW32 (Address a) = (fromIntegral a) :: Word32
    toW32 Bad = cantHappen
    binaryBits' :: Word32 -> Word32 -> Word32
    binaryBits' l r =
        case op of
          "&" -> l .&. r
          "|" -> l .|. r
          "^" -> l `xor` r
          _ -> cantHappen

    badShift = evalError opOff (printf "Bad expression: ... %s ADDRESS" op)
    notSupported = evalError opOff (printf "Not supported: ADDRESS %s ..." op)


calcValue :: [LabelDef] -> CodeAddr -> AsmExpr -> Parser Int
calcValue labelDef pos expr = do
    val <- calcExpr (Just labelDef) pos expr
    case val of
      (Address _) -> undefined -- evalError $ "address can not be used here"
      Bad -> return 0
      (Num d) -> return d

evalError :: Int -> String -> Evaluator
evalError off msg =
    tell [WithPos off msg] >> return Bad

--
-- Local Variables:
-- coding: utf-8
-- End:
