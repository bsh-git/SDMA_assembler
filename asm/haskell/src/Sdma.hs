--
--
module Sdma where

import Data.Text (Text)
import Data.Word
import Data.List
import Text.Printf
import System.IO
import Sdma.Parser
import Sdma.Codegen

-- import Debug.Trace

assembleFile :: FilePath -> Text -> Maybe Word16 -> Either String [Word16]
assembleFile filename source startAddr = do
    either reportErrors Right $ parse (asmFile >>= gen) filename source
  where
    gen insn = generate start (fixLabels start insn) insn
    reportErrors  = (Left . errorBundlePretty)
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
