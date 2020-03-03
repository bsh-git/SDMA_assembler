--
--
module Main where

import Data.Word
import Data.Text (Text)
import qualified Data.Text.IO as Txtio
import Data.List
import System.Environment
import System.IO
import qualified System.Exit
import Text.Printf
import Text.Megaparsec.Error
import Sdma.Parser
import Sdma.Codegen


main :: IO ()
main = do
    args <- getArgs

    case length args of
      0 -> Txtio.hGetContents stdin >>= processFile "(stdin)"
      1 -> Txtio.readFile (head args) >>= processFile (head args)
      _ -> die "too many argments"


die :: String -> IO ()
die msg = do
    progname <- getProgName
    System.Exit.die (progname ++ ": " ++ msg)


processFile :: FilePath -> Text -> IO ()
processFile filename contents = do
    either parseError gen $ parseSdmaAsm filename contents

  where
      parseError e = do
          hPutStrLn stderr ("Syntax error on " ++ filename)
          hPutStrLn stderr (errorBundlePretty e)
          die "abort."

      gen insns =
          either genError (writeCodes filename) $ generate 0 (fixLabels 0 insns) insns

      genError e = do
          hPutStrLn stderr ("Error on " ++ filename)
          hPutStrLn stderr e
          die "abort."

writeCodes :: FilePath -> [Word16] -> IO ()
writeCodes filename codes = do
    putStrLn $ "/* " ++ filename ++ " */"
    putStrLn "static const uint16_t sdma_code[] = {"
    writeWords codes
    putStrLn "};"

  where
    writeWords :: [Word16] -> IO ()
    writeWords [] = return ()
    writeWords w = writeWords' (splitAt 8 w)
    writeWords' (ws, rest) = do
        putStrLn $ "\t" ++ (intercalate " " $ map  (\w -> (printf "%#06x," w) :: String) ws)
        writeWords rest

--
-- Local Variables:
-- coding: utf-8
-- End:
