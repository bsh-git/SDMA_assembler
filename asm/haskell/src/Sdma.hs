--
--
module Sdma where

import Data.ByteString (ByteString)
import Data.Word
import Data.List
import Control.Monad (forM_)
import Text.Printf
import System.IO
import Sdma.Parser
import Sdma.Codegen
import Sdma.Cpp

-- import Debug.Trace

assembleFile :: FilePath -> ByteString -> Maybe Word16 -> [SourcePosInfo] -> Either String [Word16]
assembleFile filename source startAddr spi = do
    either reportErrors Right $ parse (asmFile >>= gen) filename source
  where
    gen insn = generate start (fixLabels start insn) insn
    reportErrors = Left . concat . (map  errorBundlePretty) . (splitErrorBundle spi)
    start = maybe (WordAddr Rel 0) (WordAddr Abs) startAddr

writeCodes :: FilePath -> Handle -> [Word32] -> IO ()
writeCodes sourceFilename outputHandle codes = do
    hPutStrLn outputHandle $ "/* " ++ sourceFilename ++ " */"
    hPutStrLn outputHandle "static const uint16_t sdma_code[] = {"
    writeWords codes
    hPutStrLn outputHandle "};"

  where
    writeWords :: [Word32] -> IO ()
    writeWords [] = return ()
    writeWords w = writeWords' (splitAt 8 w)
    writeWords' (ws, rest) = do
        hPutStrLn outputHandle $
          "\t" ++ (intercalate " " $ map  (\w -> (printf "%#10x," w) :: String) ws)
        writeWords rest


writeCodesAsData :: FilePath -> Handle -> [Word32] -> IO ()
writeCodesAsData _ outputHandle codes =
    forM_ codes $ \w ->
                    hPutStrLn outputHandle (printf "%08x" w)

toWord32 :: (Integral a) => a -> Word32
toWord32 w = fromIntegral w :: Word32

--
-- Local Variables:
-- coding: utf-8
-- End:
