{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
--
--

module SDMA.Read (
  parseLines,
  LineNumber
#ifdef UnitTestInternal
  , asmOperand
  , asmExpression
  , asmTerm
  , asmLine
  , asmFile
  , symbolToRegister
#endif
  ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Expr
import Data.Char
import Data.Word
import Data.Maybe
import SDMA

type LineNumber = Line

{-
symbolChars = alphaNum <|> char '_'
symbolNumChars = digit <|> symbolChars

symbol :: GenParser Char st String
symbol = do
  a <- symbolChars
  b <- many symbolNumChars
  return (a:b)
-}

lexer = PT.makeTokenParser javaStyle
-- number = PT.natural lexer
-- symbol = PT.identifier lexer

number :: GenParser Char st Int
number = try hexadecimal <|> decimal
  where
    hexadecimal =
      char '0' >> oneOf "xX" >> many1 hexDigit >>= (\s -> return (read ("0x" ++ s) :: Int))
    decimal =
      many1 digit >>= (\s -> return (read s :: Int))
      



symbol :: GenParser Char st String
symbol = do
    a <- symbolChar
    b <- many symbolNumChar
    return (a:b)
  where symbolChar = alphaNum <|> char '_'
        symbolNumChar = digit <|> symbolChar


skipBlank = skipMany (oneOf " \t")

asmLabel :: GenParser Char st String
asmLabel = do
  a <- symbol
  skipBlank
  _ <- char ':'
  skipBlank
  return a
  
--labelOrSymbol :: GenParser Char st String
--labelOrSymbol = do


asmComment :: GenParser Char st String
asmComment = 
  skipBlank >> oneOf "#;" >>  manyTill anyChar (try (lookAhead endOfLine))

asmInstruction :: GenParser Char st SDMAInstruction
asmInstruction = do
  skipBlank
  nmemonic <- symbol
  skipBlank
  left <- optionMaybe firstOperand
  if isNothing left
    then return (SDMAInstruction nmemonic Empty Empty)
    else do
            right <- try secondOperand <|> (return Empty)
            return (SDMAInstruction nmemonic (fromJust left) right)
    
  
firstOperand ::  GenParser Char st SDMAOperand
firstOperand = do
  -- _ <- many1 space     -- at least one space if we have operands
  asmOperand
  
secondOperand :: GenParser Char st SDMAOperand
secondOperand = do
  skipBlank
  _ <- char ','
  skipBlank
  asmOperand

asmFile :: GenParser Char st [([String], Maybe SDMAInstruction, Line)]
asmFile = do
  lns <- asmLine `sepBy` (char '\n')
  eof
  return lns

asmLine:: GenParser Char st ([String], Maybe SDMAInstruction, Line)
asmLine = do
  pos <- getPosition
  skipBlank
  l <- many (try asmLabel)
  skipBlank
  b <- optionMaybe asmInstruction
  optional asmComment
  return (l, b, sourceLine pos)


asmOperand :: GenParser Char st SDMAOperand
asmOperand = try indexed <|> asmExpression

asmExpression :: GenParser Char st SDMAOperand
asmExpression = buildExpressionParser exprTable asmTerm <?> "expression"
  where exprTable = [ [prefix "-", prefix "+"]
                    , [binary "*" AssocLeft, binary "/" AssocLeft]
                    , [binary "+" AssocLeft, binary "-" AssocLeft] ]

        prefix name = Prefix (do {skipBlank; _ <- string name; skipBlank; return (UnaryOp name)})
        binary name assoc = Infix (do {skipBlank; _ <- string name; skipBlank; return (BinaryOp name)}) assoc

--unaryOp name = UnaryOp name
--binOp name = BinaryOp name


--
-- parse (Rn, disp)
--
indexed :: GenParser Char st SDMAOperand
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

asmTerm :: GenParser Char st SDMAOperand
asmTerm =
      (do
          _ <- char '('
          expr <- asmExpression
          _ <- char ')'
          return expr)
  <|> (do
          n <- number
          skipBlank
          return $ Number n)
  <|> (do
          s <- symbol
          skipBlank
          return $ Symbol s)
  <?> "term"

               
symbolToRegister :: SDMAOperand -> SDMAOperand
symbolToRegister source@(Symbol s) =
  if not (length s == 2 && (head s) `elem` "rR" && (isDigit . head. tail) s )
  then
    source
  else
    Register (read (tail s) :: Word8)

symbolToRegister x = x

-- returns
--  Right (Label, instruction)
--  Left ""  -- for empty lines
--  Left "error message" -- for syntax error
parseLines :: String -> String -> Either ParseError [([String], Maybe SDMAInstruction, Line)]
parseLines filename input = parse asmFile filename input

  -- split up to (maybe label), nmemonic, operand0, operand1
--  let words = "^\s*(([a-zA-Z_][a-zA-Z_0-9]*)\s*:)?\s*



--
-- Local Variables:
-- coding: utf-8
-- End:
