--
--
module Sdma where

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Word
import Data.List
import qualified Data.ByteString.Char8 as B
import Text.Printf
import System.IO
import Sdma.Parser
import Sdma.Codegen
import Sdma.Cpp

-- import Debug.Trace

assembleFile :: FilePath -> Text -> Maybe Word16 -> [SourcePosInfo] -> Either String [Word16]
assembleFile filename source startAddr spi = do
    either reportErrors Right $ parse (asmFile >>= gen) filename source
  where
    gen insn = generate start (fixLabels start insn) insn
--    reportErrors  = (Left . errorBundlePretty)
--    reportErrors = (Left. show)
--    reportErrors = Left . show . (splitErrorBundle spi)
    reportErrors = Left . concat . (map  errorBundlePretty) . (splitErrorBundle spi)
    start = maybe (WordAddr Rel 0) (WordAddr Abs) startAddr

writeCodes :: FilePath -> Handle -> [Word16] -> IO ()
writeCodes sourceFilename outputHandle codes = do
    hPutStrLn outputHandle $ "/* " ++ sourceFilename ++ " */"
    hPutStrLn outputHandle "static const uint16_t sdma_code[] = {"
    writeWords codes
    hPutStrLn outputHandle "};"

  where
    writeWords :: [Word16] -> IO ()
    writeWords [] = return ()
    writeWords w = writeWords' (splitAt 8 w)
    writeWords' (ws, rest) = do
        hPutStrLn outputHandle $
          "\t" ++ (intercalate " " $ map  (\w -> (printf "%#06x," w) :: String) ws)
        writeWords rest



--
-- Local Variables:
-- coding: utf-8
-- End:
