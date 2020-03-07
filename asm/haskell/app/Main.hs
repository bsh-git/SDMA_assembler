--
--
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Txtio
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (decodeUtf8)
import System.Environment
import System.IO
import qualified System.Exit
import Sdma
import Sdma.Cpp


main :: IO ()
main = do
    args <- getArgs

    case length args of
      0 -> B.hGetContents stdin >>= processFile "(stdin)"
      1 -> B.readFile (head args) >>= processFile (head args)
      _ -> die $ "too many argments" ++ (show args)


die :: String -> IO ()
die msg = do
    progname <- getProgName
    System.Exit.die (progname ++ ": " ++ msg)


processFile :: FilePath -> ByteString -> IO ()
processFile filename contents = do
    spi <- cppMarks filename contents

    either reportError outputCodes $ assembleFile filename (decodeUtf8 contents) Nothing spi

  where
      reportError e = do
          hPutStrLn stderr ("Syntax error(s) on " ++ filename)
          hPutStrLn stderr e
          die "abort."

      outputCodes codes = writeCodes filename stdout codes

--
-- Local Variables:
-- coding: utf-8
-- End:
