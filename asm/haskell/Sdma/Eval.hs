--
--
module Sdma.Eval where

import Data.Word
import Sdma.Parser

type LabelDef = (Word16, [WithPos Label])

addLabelDef :: [LabelDef] -> [WithPos Label] -> Word16 -> [LabelDef]
addLabelDef ld [] _ = ld
addLabelDef [] new pos = [(pos, new)]
addLabelDef ld@((pos0, lbl):ls) new pos
    | pos0 == pos = (pos, new ++ lbl):ls   -- multiple labels at the same position in several lines
    | otherwise = (pos, new):ld
                     

findLabel :: [LabelDef] -> String -> Either String Word16
findLabel ld s = case found of
                   [] -> Left $ "Label " ++ s ++ " not found"
                   [x] -> Right $ fst x
                   _ -> Left $ "Duplicate definition of " ++ s
    where found = filter isLabelDefined ld
          isLabelDefined d = (Label s) `elem` map tokenVal (snd d)

-- (Label s) `elem` (snd d)

findLocalLabel :: [LabelDef] -> Word -> LabelRefDirection -> Word16 -> Either String Word16
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


--
-- result of evaluating expression
--
data Value = Num Int | Address Word16
    deriving (Eq, Show)

calcExpr :: [LabelDef] -> Word16 -> AsmExpr -> Either String Value
calcExpr labelDef pos expr =
    case expr of
      Leaf (WithPos _ (Number n)) -> Right (Num (fromIntegral n))
      Leaf (WithPos _ (Identifier s)) ->
          if s == "."
          then Right (Address pos)  -- `.' for current address
          else Address `fmap` findLabel labelDef s
          
      Leaf (WithPos _ (LocalLabelRef dir n)) -> 
          Address `fmap` findLocalLabel labelDef n dir pos

      Leaf (WithPos _ t) -> Left $ "Invalid token " ++ (show t)

      UnaryExpr op e -> unaryExpr labelDef pos  op e
      BinaryExpr op left right -> binaryExpr labelDef pos op left right

      Indexed _ _ -> Left $ "Invalid expression " ++ (show expr)
      Register _ -> Left $ "Invalid expression " ++ (show expr)


unaryExpr :: [LabelDef] -> Word16 -> (WithPos String) -> AsmExpr -> Either String Value
unaryExpr labelDef pos (WithPos _ op) expr = do
    expr' <- calcExpr labelDef pos expr
    case (op, expr') of
      ("+", _) -> Right $ expr'
      ("-", (Num v)) -> Right (Num (-v))
      ("-", (Address _)) -> Left "Bad expression (unary minus) to address"
      (_, _) -> Left $ "Unkonw unary operator " ++ op

binaryExpr :: [LabelDef] -> Word16 -> (WithPos String) -> AsmExpr -> AsmExpr -> Either String Value
binaryExpr labelDef pos (WithPos _ op) left right = do
    left' <- calcExpr labelDef pos left
    right' <- calcExpr labelDef pos right
    case (left', right') of
      ((Address _), (Address _)) ->
          if op /= "-"
          then Left $ "Bad expression: ADDRESS " ++ op ++ " ADDRESS"
          else binaryExpr' left' right'
      ((Address _), _) -> binaryExpr' left' right'
      (_, (Address _)) -> binaryExpr' right' left'
      (_, _) -> binaryExpr' left' right'

  where
    binaryExpr' l r =
        case (op, l, r) of
          ("+", (Address addr), (Num v)) -> Right $ Address (addr + (fromIntegral v))
          ("+", (Num l'), (Num r')) -> Right $ Num (l' + r')
          ("-", (Address l'), (Address r')) -> Right $ Address (l' - r')
          ("-", (Address addr), (Num v)) -> Right $ Address (addr - (fromIntegral v))
          ("-", (Num l'), (Num r')) -> Right $ Num (l' + r')
          (_, (Address _), _) -> Left $ "Bad expression: " ++ op ++ " to Address"
          ("*", (Num l'), (Num r')) -> Right $ Num (l' * r')
          ("/", (Num l'), (Num r')) -> Right $ Num (l' `div` r')
          ("%", (Num l'), (Num r')) -> Right $ Num (l' `mod` r')
          (_, _, _) -> Left $ "Unsupported expression: " ++ (show (op, left, right))
              
           
calcValue :: [LabelDef] -> Word16 -> AsmExpr -> Either String Int
calcValue labelDef pos expr =
    case calcExpr labelDef pos expr of
      Right (Address _) -> Left $ "address can not be used here"
      Right (Num d) -> Right d
      Left m -> Left m
      

--
-- Local Variables:
-- coding: utf-8
-- End:
