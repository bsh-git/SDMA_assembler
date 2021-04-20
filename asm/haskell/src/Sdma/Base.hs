--
--
module Sdma.Base where

import Data.Word
import Data.Void
import Data.Char
import qualified Data.ByteString as B
import Text.Megaparsec

type Parser = Parsec Void B.ByteString

--
-- Address
--  SDMA's program counter is incremented by 1 for each (non-jump) instruction.
--  Instruction codes are 16-bit wide.
--
data CodeAddr = WordAddr RelAbs Word16 -- for instructions
                | ByteAddr RelAbs Word16 -- for directives such as .ascii or .dc.b
                deriving (Show, Eq, Ord)

data RelAbs = Rel | Abs
    deriving (Show, Eq, Ord)


toByteAddr :: CodeAddr -> CodeAddr
toByteAddr a@(ByteAddr _ _) = a
toByteAddr (WordAddr r a) = ByteAddr r (a * 2)

toWordAddr :: CodeAddr -> CodeAddr
toWordAddr a@(WordAddr _ _) = a
toWordAddr (ByteAddr r a) = WordAddr r ((a + 1) `div` 2)

incrementAddr :: CodeAddr -> CodeAddr
incrementAddr (ByteAddr r a) = ByteAddr r (a+1)
incrementAddr (WordAddr r a) = WordAddr r (a+1)

advanceAddr :: CodeAddr -> Int -> CodeAddr
advanceAddr (ByteAddr r a) n = ByteAddr r (a + fromIntegral n)
advanceAddr (WordAddr r a) n = WordAddr r (a + fromIntegral n)

getAddr :: CodeAddr -> Word16
getAddr (WordAddr _ a) = a
getAddr (ByteAddr _ a) = a


alignAddr :: Int -> CodeAddr -> CodeAddr
alignAddr b (WordAddr r a) = WordAddr r $ fromIntegral $ b * (((fromIntegral a) + b - 1) `div` b)
alignAddr _ (ByteAddr _ _) = error "byte address is not allowed"


isAbsolute :: CodeAddr -> Bool
isAbsolute = (Abs == ) . getAbsRel
  where
    getAbsRel a =
        case a of
      (WordAddr r _) -> r
      (ByteAddr r _) -> r

--
--
--
--cantHappen :: Parser a
cantHappen :: a
cantHappen = error "can't happen"


--
-- helper function for ByteString
--
bsToS :: B.ByteString -> String
bsToS = (map (chr . fromIntegral)) . B.unpack

sToBs :: String -> B.ByteString
sToBs = B.pack . (map (fromIntegral . ord))


pairs :: a -> [a] -> [(a,a)]
pairs _ [] = []
pairs pad [a] = [(a,pad)]
pairs pad (a:b:xs) = (a,b) : pairs pad xs

--
-- Options
--
data AssemblerOptions = AssemblerOptions {
      optOutputFormat :: OutputFormat
    , optLoadAddr :: Maybe Word16
{- notyet
    , optCppCommand :: String
    , optNoCpp :: Bool
-}
    , optHelp :: Bool
    }
    deriving (Show, Eq)

data OutputFormat = C | Linux | Data
                  deriving (Show, Eq)


--
-- Local Variables:
-- coding: utf-8
-- End:
