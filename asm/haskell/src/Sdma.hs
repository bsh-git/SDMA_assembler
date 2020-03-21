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
import Sdma.Base

-- import Debug.Trace

assembleFile :: FilePath -> ByteString -> Maybe Word16 -> [SourcePosInfo] -> Either String [Word16]
assembleFile filename source startAddr spi =
    either reportErrors Right $ parse (asmFile >>= gen) filename source
  where
    gen insn = generate start (fixLabels start insn) insn
    reportErrors = Left . concat . (map  errorBundlePretty) . (splitErrorBundle spi)
    start = maybe (WordAddr Rel 0) (WordAddr Abs) startAddr

writeCodes :: AssemblerOptions -> FilePath -> Handle -> [Word16] -> IO ()
writeCodes opts sourceFilename outputHandle codes = do
    writeCodes'
    hPutStrLn outputHandle "};"
  where
    writeCodes' = do
      hPutStrLn outputHandle $ "/* " ++ sourceFilename ++ " " ++ addrInfo ++ " */"
      hPutStrLn outputHandle "static const uint16_t sdma_code[] = {"
      writeWords outputHandle "\t" "%#06x," codes

    addrInfo = case optLoadAddr opts of
      Nothing -> ": relocatable"
      Just a -> printf "at %#x" a




writeCodesLinux :: AssemblerOptions -> FilePath -> Handle -> [Word32] -> IO ()
writeCodesLinux opts sourceFilename outputHandle codes = do
    writeCodes'
    hPutStrLn outputHandle "};"

  where
    writeCodes' = do
      let l = show $ length  codes
      hPutStrLn outputHandle $ "static const int sdma_code_length = " ++ l ++ ";"
      hPutStrLn outputHandle $ "static const u32 sdma_code[" ++ l ++ "] = {"
      writeWords outputHandle "  " "%#010x," codes


writeWords :: (PrintfArg a) => Handle -> String -> String -> [a] -> IO ()
writeWords _ _ _ [] = return ()
writeWords h p fmt codes = writeWords' (splitAt 8 codes)
  where
    writeWords' (ws, rest) = do
      hPutStrLn h $
        p ++ (intercalate " " $ map  (\w -> (printf fmt w) :: String) ws)
      writeWords h p fmt rest


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
