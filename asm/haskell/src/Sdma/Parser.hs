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
                   , Parser
                   , asmFile
                   , findOffset
                   , parseSdmaAsm
                   , tokenize
                   , parseOperand
                   , module Text.Megaparsec
                   ) where

import Data.Text (Text, pack, unpack)
import qualified Data.Text  as T (foldl', all, head, tail)
import Data.Void
import Data.Char
import Data.List (foldl')
import Data.Functor (void)
import Data.Either
import Data.Word
import qualified Data.Set
import Control.Monad.HT (lift2, lift)
import Text.Megaparsec hiding (Label, label, match)
--import Text.Megaparsec.Error
import Text.Megaparsec.Char -- hiding (symbolChar)
--import Text.Megaparsec.Pos
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Debug

import Debug.Trace

data WithPos a = WithPos
  { startPos :: SourcePos
  --, endPos :: SourcePos
  --, tokenLength :: Int
  , startOff :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)


type Parser = Parsec Void Text

parseSdmaAsm :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [AsmLine]
parseSdmaAsm fn input = parse asmFile fn input


asmFile :: Parser [AsmLine]
asmFile = (asmLine `sepBy` (void eol <|> void semiColon) <* eof)
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
    insn <- option Nothing statement
    return $ AsmLine lbls insn

statement :: Parser (Maybe Statement)
statement = do
    notFollowedBy (void eol <|> eof)
    withRecovery recover statement'
  where
    recover e = Nothing <$ (registerParseError e >> skipMany (noneOf' "\r\n"))
    statement' = do
      insn <- withPos (identifierChars <?> "a nmemonic or a directive")
      sc
      opr <- operand `sepBy` (char ',' >> sc)
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
goodLabel = (lbl identifierChars Label
             <|> lbl (many digitChar) (buildNumericToken LocalLabel 10))
  where
    lbl :: Parser String -> (String -> Label) -> (Parser Label)
    lbl p f = f `fmap` (p <* (sc >> single ':' >> sc))

badLabel :: Parser Label
badLabel = do
    off <- getOffset
    l <- unpack `fmap` takeWhileP Nothing (not . notLabelChar) <* (sc >> single ':' >> sc)
    let e = ErrorFail "Bad label"
    registerParseError (FancyError off (Data.Set.singleton e))
    return $ BadLabel l
  where
    -- eat wider range of chars to detect bad labels
    notLabelChar c = isSpace c || c `elem` (":#;/" :: [Char])

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
    Leaf (WithPos _ o _) -> o
    UnaryExpr (WithPos _ o _) _ -> o
    BinaryExpr (WithPos _ o _) _ _ -> o
    Indexed l _ -> findOffset l
    Register (WithPos _ o _) -> o


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
    unary op = withPosExpr UnaryExpr c op

binary :: Char -> Operator Parser AsmExpr
binary  c = InfixL (binaryExpr (char c))
  where
    binaryExpr op = withPosExpr BinaryExpr c op

withPosExpr expr c op = do
  off <- getOffset
  pos <- getSourcePos
  _ <- op
  sc
  return $ expr (WithPos pos off [c])

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
    register' (WithPos pos off (Identifier regname)) =
        if length regname /= 2 || toLower (head regname) /= 'r'
        then fail "not register"
        else let regnum = digitToInt (head (tail regname))
             in
               if regnum <= 7
               then return $ Register (WithPos pos off (fromIntegral regnum))
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
  off <- getOffset
  a <- p
  return $ WithPos pos off a

lexer :: Parser [WithPos AsmToken]
lexer = do
  blanks
  a <- many (lexeme (eols <|> identifier <|> numberOrLocalLabelRef  <|> symbolToken <|> unknown))
  eof
  return a

lexeme :: Parser a -> Parser (WithPos a)
lexeme p = do
    offBefore <- getOffset
    posBefore <- getSourcePos
    a <- p
    sc
    return $ WithPos posBefore offBefore a

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

    digitsToNumber base = buildNumericToken Number base
    digitsToLabel dir = buildNumericToken (LocalLabelRef dir) 10

buildNumericToken :: (Word -> a) -> Int -> String -> a
buildNumericToken t base = t . fromIntegral . (foldl' (\sm d -> sm * base + digitToInt d) 0)

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