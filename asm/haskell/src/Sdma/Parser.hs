{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
--
--
module Sdma.Parser ( AsmExpr(..)
                   , AsmToken(..)
                   , WithPos(..)
                   , Label(..)
                   , LabelRefDirection(..)
                   , AsmLine(..)
                   , Statement(..)
                   , Parser
                   , asmFile
                   , findOffset
                   , parseSdmaAsm
                   , tokenize
                   , parseOperand
                   , module Text.Megaparsec
                   ) where

import Data.Void
import Data.Char
import Data.Functor (void)
import Data.Word
import qualified Data.Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad.HT (lift2)
import Text.Megaparsec hiding (Label, label, match)
import Text.Megaparsec.Byte -- hiding (symbolChar)
import qualified Text.Megaparsec.Byte.Lexer as L
import Control.Monad.Combinators.Expr
import Sdma.Base

--import Debug.Trace
--import Text.Megaparsec.Debug

data WithPos a = WithPos
  { startOff :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)


parseSdmaAsm :: FilePath -> ByteString -> Either (ParseErrorBundle ByteString Void) [AsmLine]
parseSdmaAsm fn input = parse asmFile fn input


asmFile :: Parser [AsmLine]
asmFile = (asmLine `sepBy` (void eol <|> void semiColon) <* eof)
  where
    semiColon = some (singleC  ';')

--
-- statement
--
--  label: label2: op exp1, exp2 #comment
--
data AsmLine = AsmLine
    { alLabels :: [WithPos Label]
    , alStatement :: Maybe Statement
    } deriving (Eq, Show)


data Statement = Statement
    { stNmemonic :: (WithPos String)
    , stOperands :: [AsmExpr]
    } deriving (Eq, Show)

asmLine :: Parser AsmLine
asmLine = do
    sc
    lbls <- (many (try label))
    insn <- option Nothing statement
    return $ AsmLine lbls insn

statement :: Parser (Maybe Statement)
statement = do
    notFollowedBy (void eol <|> eof)
    withRecovery recover statement'
  where
    recover e = Nothing <$ (registerParseError e >> takeWhilePC Nothing (`notElem` ("\r\n" :: String)))
    statement' = do
      insn <- withPos (bsToS `fmap` identifierChars <?> "a nmemonic or a directive")
      sc
      opr <- operand `sepBy` (singleC ',' >> sc)
      return $ Just $ Statement insn opr

--
--  Label
--  foo:
--  123:    (local label)
--
data Label = Label String
           | LocalLabel Word
           | BadLabel String
    deriving (Eq, Show)



label :: Parser (WithPos Label)
label = withPos (try goodLabel <|> badLabel)

goodLabel :: Parser Label
goodLabel = (lbl identifierChars mkLabel
             <|> lbl (takeWhilePC Nothing isDigit) (buildNumericToken LocalLabel 10))
  where
    lbl :: Parser ByteString -> (ByteString -> Label) -> (Parser Label)
    lbl p f = f `fmap` (p <* (sc >> singleC ':' >> sc))
    mkLabel :: ByteString -> Label
    mkLabel s = Label (bsToS s)

badLabel :: Parser Label
badLabel = do
    off <- getOffset
    l <- takeWhilePC Nothing (not . notLabelChar) <* (sc >> singleC ':' >> sc)
    let e = ErrorFail "Bad label"
    registerParseError (FancyError off (Data.Set.singleton e))
    return $ BadLabel (bsToS l)
  where
    -- eat wider range of chars to detect bad labels
    notLabelChar c = isSpace c || c `elem` (":#;/" :: String)

--
-- Expression
--
data AsmExpr
  = Leaf (WithPos AsmToken)
  | UnaryExpr (WithPos String) AsmExpr
  | BinaryExpr (WithPos String) AsmExpr AsmExpr
  -- followings are allowed only at the top level
  | Indexed AsmExpr AsmExpr -- (Rn, N):
  | Register (WithPos Word8)
  deriving (Eq, Show)


findOffset :: AsmExpr -> Int
findOffset expr =
  case expr of
    Leaf (WithPos o _) -> o
    UnaryExpr (WithPos o _) _ -> o
    BinaryExpr (WithPos o _) _ _ -> o
    Indexed l _ -> findOffset l
    Register (WithPos o _) -> o


parseOperand :: FilePath -> ByteString -> Either (ParseErrorBundle ByteString Void) AsmExpr
parseOperand fn input = parse (blanks >> operand <* eof) fn input


operatorTable :: [[Operator Parser AsmExpr]]
operatorTable =
    [ [ prefix '-'
      , prefix '+'
      ]
    , [ binary '*'
      , binary '/'
      , binary '%'
      ]
    , [ binary '+'
      , binary '-'
      ]
    ]

prefix :: Char -> Operator Parser AsmExpr
prefix c = Prefix (unary (singleC c)) -- (apply unary (char c))
  where
    unary op = withPosExpr UnaryExpr c op

binary :: Char -> Operator Parser AsmExpr
binary  c = InfixL (binaryExpr (singleC c))
  where
    binaryExpr op = withPosExpr BinaryExpr c op

withPosExpr expr c op = do
  off <- getOffset
  _ <- op
  sc
  return $ expr (WithPos off [c])

expr :: Parser AsmExpr
expr = makeExprParser term operatorTable

term :: Parser AsmExpr
term = tokenToExpr identifier <|> tokenToExpr numberOrLocalLabelRef <|> parens expr

parens :: Parser AsmExpr -> Parser AsmExpr
parens = between (singleC '(' >> sc) (singleC ')' >> sc)

tokenToExpr p = do
  a <- lexeme p
  return $ Leaf a

operand :: Parser AsmExpr
operand = try (parens indexedAddressing) <|> try register <|> expr
  where
    indexedAddressing = do
        reg <- register
        void $ singleC ','
        sc
        e <- expr
        return $ Indexed reg e


register :: Parser AsmExpr
register = lexeme identifier >>= register'
  where
    register' (WithPos off (Identifier regname)) =
        if B.length regname /= 2 || toLower (chr (fromIntegral (B.head regname))) /= 'r'
        then fail "not register"
        else let regnum = digitToIntB (B.head (B.tail regname))
             in
               if regnum <= 7
               then return $ Register (WithPos off (fromIntegral regnum))
               else fail "invalid register number"
    register' _ = fail "not register"

--
-- Token
--
data AsmToken
  = Eol
  | Identifier ByteString
  | Number Word
  | LocalLabelRef LabelRefDirection Word
  -- Items below are defined to tokenize all input. they are not used by the parser above.
  | Symbol ByteString
  | Unknown Char
  deriving (Eq, Show)

data LabelRefDirection = Forward | Backward
    deriving (Eq, Show)

tokenize :: FilePath -> ByteString -> [WithPos AsmToken]
tokenize fn input = either parseError id $ parse lexer fn input
  where
    parseError e = error (errorBundlePretty e)


isBlank :: Word8 -> Bool
isBlank = isBlankC . chr . fromIntegral

isBlankC :: Char -> Bool
isBlankC c = c /= '\n' && c /= '\r' && isSpace c

--blank1 :: (MonadParsec e s m, Token s ~ Char) => m ()
blank1 :: Parser ()
blank1 = void $ takeWhile1P (Just "white space") isBlank

blanks :: Parser ()
blanks = void $ takeWhileP Nothing isBlank

sc :: Parser ()
sc = L.space blank1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

withPos :: Parser a -> Parser (WithPos a)
withPos p = do
  pos <- getSourcePos
  off <- getOffset
  a <- p
  return $ WithPos off a

lexer :: Parser [WithPos AsmToken]
lexer = do
  blanks
  a <- many (lexeme (eols <|> identifier <|> numberOrLocalLabelRef  <|> symbolToken <|> unknown))
  eof
  return a

lexeme :: Parser a -> Parser (WithPos a)
lexeme p = do
    offBefore <- getOffset
    a <- p
    sc
    return $ WithPos offBefore a

--symbol :: Text -> Parser Text
--symbol s = dbg "symbol" $ L.symbol empty s


eols :: Parser AsmToken
eols = some eol >> return Eol

symbolToken :: Parser AsmToken
--symbolToken = oneOf "()-+*%/;" >>= (return . Symbol . B.singleton)
symbolToken = satisfy (`B.elem` "()-+*%/;") >>= (return . Symbol . B.singleton)

identifier :: Parser AsmToken
identifier = Identifier `fmap` identifierChars

identifierChars :: Parser ByteString
identifierChars = lift2 B.cons idChar0 idChars
  where
    idChar0 = satisfy isIdChar0
    idChars = takeWhilePC Nothing isIdChar


isIdChar0 :: Word8 -> Bool
isIdChar0 = isIdChar0C . chr . fromIntegral

isIdChar0C :: Char -> Bool
isIdChar0C c = isAlpha c || c `elem` ("_." :: String)

isIdChar :: Char -> Bool
isIdChar c = isIdChar0C c || isDigit c

numberOrLocalLabelRef :: Parser AsmToken
numberOrLocalLabelRef = try hexadecimalNumber <|> try binaryNumber <|> decimalNumberOrLabelRef
  where
    hexadecimalNumber =
      singleC '0' >> oneOf' "Xx"
        >> takeWhile1PC (Just "hexadecimal digit character") isHexDigit
        >>= return . (digitsToNumber 16)

    binaryNumber = do
      singleC '0' >> oneOf' "Bb"
          >> takeWhile1PC (Just "0 or 1") (`elem` ("01" :: String)) >>= return . (digitsToNumber 2)

    decimalNumberOrLabelRef = do
        d <- takeWhile1PC (Just "digits") isDigit
        suffix <- takeWhilePC Nothing isAlphaNum
        case suffix of
          "" -> return $ digitsToNumber 10 d
          "f" -> return $ digitsToLabel Forward d
          "b" -> return $ digitsToLabel Backward d
          _ -> fail $ "Bad number " ++ (bsToS (B.append d suffix))

    digitsToNumber :: Int -> ByteString -> AsmToken
    digitsToNumber base = buildNumericToken Number base
    digitsToLabel dir = buildNumericToken (LocalLabelRef dir) 10

buildNumericToken :: (Word -> a) -> Int -> ByteString -> a
buildNumericToken t base = t . fromIntegral . (B.foldl' (\sm d -> sm * base + digitToIntB d) 0)

unknown :: Parser AsmToken
unknown = anySingle <* skipMany (noneOf' "\r\n") >>= return . Unknown . chr . fromIntegral


singleC :: Char -> Parser Word8
singleC = single . fromIntegral . ord

takeWhilePC :: Maybe String -> (Char -> Bool) -> Parser ByteString
takeWhilePC s f = takeWhileP s (f . chr . fromIntegral)

takeWhile1PC :: Maybe String -> (Char -> Bool) -> Parser ByteString
takeWhile1PC s f = takeWhile1P s (f . chr . fromIntegral)

digitToIntB :: Word8 -> Int
digitToIntB b = digitToInt (chr (fromIntegral b))

oneOf' :: ByteString -> Parser (Token ByteString)
oneOf' cs = satisfy (`B.elem` cs)
noneOf' :: ByteString -> Parser (Token ByteString)
noneOf' cs = satisfy (`B.notElem` cs)
--
-- Local Variables:
-- coding: utf-8
-- End:
