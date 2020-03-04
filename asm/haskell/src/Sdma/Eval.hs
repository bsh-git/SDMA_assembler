--
--
module Sdma.Eval where

import Data.Word
import Data.Set (singleton)
import Text.Printf
import Sdma.Parser

type LabelDef = (Word16, [WithPos Label])

addLabelDef :: [LabelDef] -> [WithPos Label] -> Word16 -> [LabelDef]
addLabelDef ld [] _ = ld
addLabelDef [] new pos = [(pos, new)]
addLabelDef ld@((pos0, lbl):ls) new pos
    | pos0 == pos = (pos, new ++ lbl):ls   -- multiple labels at the same position in several lines
    | otherwise = (pos, new):ld


findLabel :: [LabelDef] -> String -> Maybe Word16
findLabel ld s = case found of
                   [] -> Nothing
                   [x] -> Just $ fst x
                   _ -> Nothing
    where found = filter isLabelDefined ld
          isLabelDefined d = (Label s) `elem` map tokenVal (snd d)

-- (Label s) `elem` (snd d)

findLocalLabel :: [LabelDef] -> Word -> LabelRefDirection -> Word16 -> Maybe Word16
findLocalLabel ld n dir pos =
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
data Value = Num Int | Address Word16
    deriving (Eq, Show)

calcExpr :: [LabelDef] -> Word16 -> AsmExpr -> Parser Value
calcExpr labelDef pos expr =
    case expr of
      Leaf (WithPos _ _ (Number n)) -> return (Num (fromIntegral n))
      Leaf (WithPos _ off (Identifier s)) ->
          if s == "."
          then return (Address pos)  -- `.' for current address
          else case (findLabel labelDef s) of
                 Nothing -> evalError off $ printf "not defined %s" s
                 Just a -> return (Address a)

      Leaf (WithPos _ off (LocalLabelRef dir n)) ->
          case findLocalLabel labelDef n dir pos of
            Nothing -> evalError off $ "label not defined"
            Just a -> return (Address a)

      Leaf (WithPos _ off t) -> evalError off $ "Invalid token " ++ (show t)

      UnaryExpr op e -> unaryExpr labelDef pos  op e
      BinaryExpr op left right -> binaryExpr labelDef pos op left right

      (Indexed _ _) -> evalError (findOffset expr) $ "Invalid expression "
      (Register _) -> evalError (findOffset expr) $ "Invalid expression "


unaryExpr :: [LabelDef] -> Word16 -> (WithPos String) -> AsmExpr -> Parser Value
unaryExpr labelDef pos (WithPos _ opOff op) expr = do
    expr' <- calcExpr labelDef pos expr
    case (op, expr') of
      ("+", _) -> return $ expr'
      ("-", (Num v)) -> return (Num (-v))
      ("-", (Address a)) -> -- evaluate as (Address 0) - (Address a)
        return (Num (- (fromIntegral a)))
      (_, _) -> evalError opOff $ "Unkonw unary operator " ++ op

binaryExpr :: [LabelDef] -> Word16 -> (WithPos String) -> AsmExpr -> AsmExpr -> Parser Value
binaryExpr labelDef pos (WithPos _ opOff op) left right = do
    left' <- calcExpr labelDef pos left
    right' <- calcExpr labelDef pos right
    case (left', right') of
      ((Address _), (Address _)) ->
          if op /= "-"
          then evalError opOff $ "Bad expression: ADDRESS " ++ op ++ " ADDRESS"
          else binaryExpr' left' right'
      ((Address _), _) -> binaryExpr' left' right'
      (_, (Address _)) -> binaryExpr' right' left'
      (_, _) -> binaryExpr' left' right'

  where
    binaryExpr' l r =
        case (op, l, r) of
          ("+", (Address addr), (Num v)) -> return $ Address (addr + (fromIntegral v))
          ("+", (Num l'), (Num r')) -> return $ Num (l' + r')
          ("-", (Address l'), (Address r')) -> return $ Address (l' - r')
          ("-", (Address addr), (Num v)) -> return $ Address (addr - (fromIntegral v))
          ("-", (Num l'), (Num r')) -> return $ Num (l' + r')
          (_, (Address _), _) -> evalError opOff $ "Bad expression: " ++ op ++ " to Address"
          ("*", (Num l'), (Num r')) -> return $ Num (l' * r')
          ("/", (Num l'), (Num r')) -> return $ Num (l' `div` r')
          ("%", (Num l'), (Num r')) -> return $ Num (l' `mod` r')
          (_, _, _) -> evalError opOff $ "Unsupported expression: " ++ (show (op, left, right))


calcValue :: [LabelDef] -> Word16 -> AsmExpr -> Parser Int
calcValue labelDef pos expr = do
    val <- calcExpr labelDef pos expr
    case val of
      (Address _) -> undefined -- evalError $ "address can not be used here"
      (Num d) -> return d

evalError :: Int -> String -> Parser Value
evalError off msg = do
  registerParseError $ FancyError off $ Data.Set.singleton (ErrorFail msg)
  return (Num 0)

--
-- Local Variables:
-- coding: utf-8
-- End: