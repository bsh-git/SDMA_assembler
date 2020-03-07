--
--
module Main where

import Data.Text (Text)
import qualified Data.Text.IO as Txtio
import System.Environment
import System.IO
import qualified System.Exit
import Sdma


main :: IO ()
main = do
    args <- getArgs

    case length args of
      0 -> Txtio.hGetContents stdin >>= processFile "(stdin)"
      1 -> Txtio.readFile (head args) >>= processFile (head args)
      _ -> die $ "too many argments" ++ (show args)


die :: String -> IO ()
die msg = do
    progname <- getProgName
    System.Exit.die (progname ++ ": " ++ msg)


processFile :: FilePath -> Text -> IO ()
processFile filename contents = do
    either reportError outputCodes $ assembleFile filename contents Nothing

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
