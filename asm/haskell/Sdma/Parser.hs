{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
--
--
module Sdma.Parser ( AsmExpr(..)
                   , AsmToken(..)
                   , WithPos(..)
                   , tokenize
                   , parseExpr
                   ) where

import Data.Text (Text)
import Data.Void
import Data.Char
import Data.List (foldl')
import Data.Functor (void)
import Control.Monad.HT (lift2)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (symbolChar)
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

--
-- Expression
--
data AsmExpr
  = Leaf (WithPos AsmToken)
  | UnaryExpr (WithPos String) AsmExpr
  | BinaryExpr (WithPos String) AsmExpr AsmExpr
  deriving (Eq, Show)


parseExpr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) AsmExpr
parseExpr fn input = parse (blanks >> expr) fn input


operatorTable :: [[Operator Parser AsmExpr]]
operatorTable =
    [ [ prefix '-'
      , prefix '+'
      ]
    , [ binary '*'
      , binary '/'
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
term = tokenToExpr identifier <|> tokenToExpr number

tokenToExpr p = do
  a <- lexeme p
  return $ Leaf a

--
-- Token
--
data AsmToken
  = Eol
  | Identifier String
  | Number Word
  | LocalLabelRef Word
  -- Items below are defined to tokenize all input. they are not used by the parser above.
  | Symbol String
  | Unknown Char
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
  a <- many (lexeme (eols <|> identifier <|> number  <|> symbolToken <|> unknown))
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
identifier = lift2 (:) idChar0 (many idChar) >>= (return . Identifier)
  where
    idChar0 = letterChar  <|> oneOf' "_."
    idChar = idChar0 <|> digitChar

number :: Parser AsmToken
number = try hexadecimalNumber <|> try binaryNumber <|> decimalNumber
  where
    hexadecimalNumber = 
      char '0' >> oneOf' "Xx" >> some hexDigitChar >>= return . (digitsToNumber 16)

    binaryNumber = do
      char '0' >> oneOf' "Bb" >> some (oneOf' "01") >>= return . (digitsToNumber 2)

    decimalNumber = some digitChar >>= return . (digitsToNumber 10)
  
    digitsToNumber base = Number . fromIntegral . (foldl' (\sm d -> sm * base + digitToInt d) 0)

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
