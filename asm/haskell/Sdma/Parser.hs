{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
--
--
module Sdma.Parser ( AsmExpr(..)
                   , AsmToken(..)
                   , WithPos(..)
                   , Label(..)
                   , LabelRefDirection(..)
                   , AsmLine(..)
                   , Statement(..)
                   , parseSdmaAsm
                   , tokenize
                   , parseOperand
                   , module Text.Megaparsec
                   ) where

import Data.Text (Text)
import Data.Void
import Data.Char
import Data.List (foldl')
import Data.Functor (void)
import Data.Either
import Data.Text (append, unpack)
import Data.Word
import Control.Monad.HT (lift2)
import Text.Megaparsec hiding (Label, label)
--import Text.Megaparsec.Error
import Text.Megaparsec.Char -- hiding (symbolChar)
--import Text.Megaparsec.Pos
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Debug

data WithPos a = WithPos
  { startPos :: SourcePos
  --, endPos :: SourcePos
  --, tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)


type Parser = Parsec Void Text

parseSdmaAsm :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [AsmLine]
parseSdmaAsm fn input = parse (asmFile <* eof) fn input


asmFile :: Parser [AsmLine]
asmFile = asmLine `sepBy` (void eol <|> void semiColon)
  where
    semiColon = some (char ';')

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
    insn <- option Nothing (try statement)
    return $ AsmLine lbls insn

statement :: Parser (Maybe Statement)
statement = do
    insn <- withPos identifierChars
    sc
    opr <- operand `sepBy` (char ',' >> sc)
    return $ Just $ Statement insn opr

--
--  Label
--  foo:
--  123:    (local label)
--
data Label = Label String
           | LocalLabel Int
    deriving (Eq, Show)


label :: Parser (WithPos Label)
label = withPos (label' identifierChars Label
                    <|> label' (many digitChar) (buildToken LocalLabel 10))
  where
    label' :: Parser String -> (String -> Label) -> (Parser Label)
    label' p f = f `fmap` (p <* (sc >> char ':' >> sc))



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


parseOperand :: FilePath -> Text -> Either (ParseErrorBundle Text Void) AsmExpr
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
prefix c = Prefix (unary (char c)) -- (apply unary (char c))
  where
    unary op = do
        pos <- getSourcePos
        _ <- op
        sc
        return $ UnaryExpr (WithPos pos [c])

binary :: Char -> Operator Parser AsmExpr
binary  c = InfixL (binaryExpr (char c))
  where
    binaryExpr op = do
        pos <- getSourcePos
        _ <- op
        sc
        return $ BinaryExpr (WithPos pos [c])
expr :: Parser AsmExpr
expr = makeExprParser term operatorTable

term :: Parser AsmExpr
term = tokenToExpr identifier <|> tokenToExpr numberOrLocalLabelRef <|> parens expr

parens :: Parser AsmExpr -> Parser AsmExpr
parens = between (char '(' >> sc) (char ')' >> sc)

tokenToExpr p = do
  a <- lexeme p
  return $ Leaf a

operand :: Parser AsmExpr
operand = try (parens indexedAddressing) <|> try register <|> expr
  where
    indexedAddressing = do
        reg <- register
        void $ char ','
        sc
        e <- expr
        return $ Indexed reg e


register :: Parser AsmExpr
register = lexeme identifier >>= register'
  where
    register' (WithPos pos (Identifier regname)) =
        if length regname /= 2 || toLower (head regname) /= 'r'
        then fail "not register"
        else let regnum = digitToInt (head (tail regname))
             in
               if regnum <= 7
               then return $ Register (WithPos pos (fromIntegral regnum))
               else fail "invalid register number"
    register' _ = fail "not register"
                

    

--
-- Token
--
data AsmToken
  = Eol
  | Identifier String
  | Number Word
  | LocalLabelRef LabelRefDirection Word
  -- Items below are defined to tokenize all input. they are not used by the parser above.
  | Symbol String
  | Unknown Char
  deriving (Eq, Show)

data LabelRefDirection = Forward | Backward
    deriving (Eq, Show)

tokenize :: FilePath -> Text -> [WithPos AsmToken]
tokenize fn input = either parseError id $ parse lexer fn input
  where
    parseError e = error (errorBundlePretty e)


isBlank :: Char -> Bool
isBlank c = c /= '\n' && c /= '\r' && isSpace c

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
  a <- p
  return $ WithPos pos a


lexer :: Parser [WithPos AsmToken]
lexer = do
  blanks
  a <- many (lexeme (eols <|> identifier <|> numberOrLocalLabelRef  <|> symbolToken <|> unknown))
  eof
  return a

lexeme :: Parser a -> Parser (WithPos a)
lexeme p = do
    posBefore <- getSourcePos
    a <- p
    sc
    return $ WithPos posBefore a

--symbol :: Text -> Parser Text
--symbol s = dbg "symbol" $ L.symbol empty s


eols :: Parser AsmToken
eols = some eol >> return Eol

symbolToken :: Parser AsmToken
symbolToken = oneOf' "()-+*%/;" >>= (return . Symbol . (:[]))

identifier :: Parser AsmToken
identifier = Identifier `fmap` identifierChars

identifierChars :: Parser String
identifierChars = lift2 (:) idChar0 (many idChar)
  where
    idChar0 = letterChar  <|> oneOf' "_."
    idChar = idChar0 <|> digitChar

numberOrLocalLabelRef :: Parser AsmToken
numberOrLocalLabelRef = try hexadecimalNumber <|> try binaryNumber <|> decimalNumberOrLabelRef
  where
    hexadecimalNumber = 
      char '0' >> oneOf' "Xx" >> some hexDigitChar >>= return . (digitsToNumber 16)

    binaryNumber = do
      char '0' >> oneOf' "Bb" >> some (oneOf' "01") >>= return . (digitsToNumber 2)

    decimalNumberOrLabelRef = do
        d <- some digitChar
        suffix <- many alphaNumChar
        case suffix of
          "" -> return $ digitsToNumber 10 d
          "f" -> return $ digitsToLabel Forward d
          "b" -> return $ digitsToLabel Backward d
          _ -> fail $ "Bad number " ++ d ++ suffix
  
    digitsToNumber base = buildToken Number base
    digitsToLabel dir = buildToken (LocalLabelRef dir) 10

buildToken t base = t . fromIntegral . (foldl' (\sm d -> sm * base + digitToInt d) 0)

unknown :: Parser AsmToken
unknown = anySingle <* skipMany (noneOf' "\r\n") >>= return . Unknown



--
-- resolv anbiguity
--
oneOf' :: [Char] -> Parser Char
oneOf' s = oneOf s
noneOf' :: [Char] -> Parser Char
noneOf' s = noneOf s


--
-- Local Variables:
-- coding: utf-8
-- End:
