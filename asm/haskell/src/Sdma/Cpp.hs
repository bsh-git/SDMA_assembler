{-# LANGUAGE OverloadedStrings #-}
--
--
module Sdma.Cpp where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable as F (foldlM, foldl')
import Data.Array
--import Data.Text (Text)
import Data.Void
import qualified Data.List.NonEmpty as NE
import Text.Regex.PCRE.ByteString
import Text.Megaparsec
import Text.Megaparsec.Error

data SourcePosInfo = SourcePosInfo {
    spiSrcOff   :: Int
  , spiFile     :: FilePath
  , spiFileLine  :: Int
  }
  deriving (Show, Eq)


initSourcePosInfo :: FilePath -> [SourcePosInfo]
initSourcePosInfo filename = [SourcePosInfo 0 filename 1]

cppMarks :: FilePath -> ByteString -> IO ([SourcePosInfo])
cppMarks filename input = do
    Right regex <- compile 0 0 "^# (\\d+) \"([^\"]+)\""
    (_, i) <- F.foldlM (f regex) (0, initSourcePosInfo filename) (B.lines input)
    return (reverse i)
  where
    f :: Regex -> (Int, [SourcePosInfo]) -> ByteString -> IO (Int, [SourcePosInfo])
    f regex (o, i) line = do
        Right result <- execute regex line
        let o' = o + 1 + B.length line
        case result of
          Nothing -> return (o', i)
          Just r -> let lineno = read $ B.unpack $ getCapture line (r ! 1)
                        fname = B.unpack (getCapture line (r ! 2))
                    in return (o', (SourcePosInfo o' fname lineno):i)
    getCapture line (off,len) = B.take len (B.drop off line)

--
-- split an error bundle to some, grouping based on the original source before preprocessed.
--
splitErrorBundle :: [SourcePosInfo] -> ParseErrorBundle ByteString Void -> [ParseErrorBundle ByteString Void]
splitErrorBundle spInfo bundle = reverse $ snd $ foldl' f (spInfo,[]) (bundleErrors bundle)
  where
    f :: ([SourcePosInfo], [ParseErrorBundle ByteString Void]) -> ParseError ByteString Void
      -> ([SourcePosInfo], [ParseErrorBundle ByteString Void])
    f (sis,bs) pe = let sis' = dropSourcePosInfo (errorOffset pe) sis
                        si = head sis'
                        pe' = setErrorOffset (errorOffset pe - spiSrcOff si) pe
                        tweakPosState p = p {
                              pstateInput = B.drop (spiSrcOff si) (pstateInput p)
                            , pstateSourcePos = (pstateSourcePos p) {
                                    sourceName = (spiFile si)
                                  , sourceLine = mkPos (spiFileLine si)
                                  , sourceColumn = mkPos 1
                                  }
                            }
                   in
                     (sis', (ParseErrorBundle (NE.fromList [pe'])
                             (tweakPosState (bundlePosState bundle)))
                           :bs)

                     
dropSourcePosInfo :: Int -> [SourcePosInfo] -> [SourcePosInfo]
dropSourcePosInfo _ [] = error "can't happen"
dropSourcePosInfo _ sis@[_] = sis
dropSourcePosInfo off sis@(_:si1:_)
    | (spiSrcOff si1) <= off = dropSourcePosInfo off (tail sis)
    | otherwise = sis


--
-- Local Variables:
-- coding: utf-8
-- End:
