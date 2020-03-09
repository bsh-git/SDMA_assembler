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
assembleFile filename source startAddr spi = do
    either reportErrors Right $ parse (asmFile >>= gen) filename source
  where
    gen insn = generate start (fixLabels start insn) insn
    reportErrors = Left . concat . (map  errorBundlePretty) . (splitErrorBundle spi)
    start = maybe (WordAddr Rel 0) (WordAddr Abs) startAddr

writeCodes :: AssemblerOptions -> FilePath -> Handle -> [Word32] -> IO ()
writeCodes opts sourceFilename outputHandle codes = do
    if Linux == optOutputFormat opts then writeCodesLinux else writeCodes'

    hPutStrLn outputHandle "};"

  where
    writeCodes' = do
      hPutStrLn outputHandle $ "/* " ++ sourceFilename ++ " " ++ addrInfo ++ " */"
      hPutStrLn outputHandle "static const uint32_t sdma_code[] = {"
      writeWords "\t" codes

    addrInfo = case optLoadAddr opts of
      Nothing -> ": relacatable"
      Just a -> printf "at %#x" a


    writeCodesLinux = do
      let l = show $ length  codes
      hPutStrLn outputHandle $ "static const int sdma_code_length = " ++ l ++ ";"
      hPutStrLn outputHandle $ "static const u32 sdma_code[" ++ l ++ "] = {"
      writeWords "  " codes

    writeWords :: String -> [Word32] -> IO ()
    writeWords _ [] = return ()
    writeWords p w = writeWords' p (splitAt 8 w)
    writeWords' prefix (ws, rest) = do
        hPutStrLn outputHandle $
          prefix ++ (intercalate " " $ map  (\w -> (printf "%#010x," w) :: String) ws)
        writeWords prefix rest


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
