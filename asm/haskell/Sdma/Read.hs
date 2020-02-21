{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
--
--

module Sdma.Read (
    parseLines
  , LineNumber
  , symbolToRegister
#ifdef UnitTestInternal
  , asmOperand
  , asmExpression
  , asmTerm
  , asmLine
  , asmFile
#endif
  ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Char
import Data.Word
import Data.Maybe
import Data.Bifunctor (bimap)
import Sdma

type LineNumber = Line

symbol :: Parser String
symbol = do
    a <- symbolChar
    b <- many symbolNumChar
    return (a:b)
  where symbolChar = alphaNum <|> char '_'
        symbolNumChar = digit <|> symbolChar


skipBlank :: Parser ()
skipBlank = skipMany (oneOf " \t")

--
--  Label
--  foo:
--  123:    (local label)
--
asmLabel :: Parser String
asmLabel = do
  a <- symbol <|> many digit
  skipBlank
  _ <- char ':'
  skipBlank
  return a
  
asmComment :: Parser String
asmComment = 
    char '#' >> many (noneOf "\n\r")

asmInstruction :: Parser SdmaInstruction
asmInstruction = do
  skipBlank
  nmemonic <- symbol
  skipBlank
  left <- optionMaybe firstOperand
  if isNothing left
    then return (SdmaInstruction nmemonic Sdma.Empty Sdma.Empty)
    else do
            right <- try secondOperand <|> (return Sdma.Empty)
            return (SdmaInstruction nmemonic (fromJust left) right)
    
  
firstOperand ::  Parser SdmaOperand
firstOperand = do
  -- _ <- many1 space     -- at least one space if we have operands
  asmOperand
  
secondOperand :: Parser SdmaOperand
secondOperand = do
  skipBlank
  _ <- char ','
  skipBlank
  asmOperand

asmFile :: Parser [([String], Maybe SdmaInstruction, Line)]
asmFile = do
  asmLine `sepBy` endOfLine

asmLine:: Parser ([String], Maybe SdmaInstruction, Line)
asmLine = do
  pos <- getPosition
  skipBlank
  l <- many (try asmLabel)
  skipBlank
  b <- optionMaybe asmInstruction
  optional asmComment
  g <- many (noneOf "\r\n")
  if null g
      then return (l, b, sourceLine pos)
      else unexpected g


asmOperand :: Parser SdmaOperand
asmOperand = try indexed <|> asmExpression

asmExpression :: Parser SdmaOperand
asmExpression = buildExpressionParser exprTable asmTerm <?> "expression"
  where exprTable = [ [prefix "-", prefix "+"]
                    , [binary "*" AssocLeft, binary "/" AssocLeft]
                    , [binary "+" AssocLeft, binary "-" AssocLeft] ]

        prefix name = Prefix (do {skipBlank; _ <- string name; skipBlank; return (UnaryOp name)})
        binary name assoc = Infix (do {skipBlank; _ <- string name; skipBlank; return (BinaryOp name)}) assoc

--
-- parse (Rn, disp)
--
indexed :: Parser SdmaOperand
indexed = do
  skipBlank
  _ <- char '('
  skipBlank
  reg <- symbol
  _ <- char ','
  skipBlank
  expr <- asmExpression
  skipBlank
  _ <- char ')'
  case symbolToRegister (Symbol reg) of
    Register n -> return (Indexed n expr)
    _ -> fail ""

asmTerm :: Parser SdmaOperand
asmTerm = do
    t <- term
    skipBlank
    return t
  where
      term = (do
                _ <- char '('
                skipBlank
                expr <- asmExpression
                _ <- char ')'
                return expr)
             <|> numberOrLocalLabelRef
             <|> (symbol >>= (return . Symbol))
             <?> "term"

               
numberOrLocalLabelRef :: Parser SdmaOperand
numberOrLocalLabelRef = try hexadecimal <|> try labelRef <|> decimal
  where
    hexadecimal =
      char '0' >> oneOf "xX" >> many1 hexDigit >>= (\s -> return (Number (read ("0x" ++ s) :: Int)))
    decimal =
      many1 digit >>= (\s -> return (Number (read s :: Int)))
    labelRef = do
        d <- many1 digit
        c <- oneOf "fFbB"
        return $ LabelRef (read d :: Int) (if c `elem` "fF" then Forward else Backward)

symbolToRegister :: SdmaOperand -> SdmaOperand
symbolToRegister source@(Symbol s) =
  if not (length s == 2 && (head s) `elem` "rR" && (isDigit . head. tail) s )
  then
    source
  else
    Register (read (tail s) :: Word8)

symbolToRegister x = x

-- returns
--  Right (Labels, instruction, linenumber)
--  Left errorinfo -- for syntax error
parseLines :: String -> String -> Either ParseError [([String], Maybe SdmaInstruction, LineNumber)]
parseLines filename input =
    -- filter out empty lines
    bimap id (filter (uncurry (||) . (bimap (not . null) isJust) . tripleToPair))
    $ parse asmFile filename input
  where
    tripleToPair (a, b, _) = (a ,b)
--  bimap id (filter (\(l,s,_) -> ((not . null) l) || (isJust s))) $ parse asmFile filename input

--
-- Local Variables:
-- coding: utf-8
-- End:
