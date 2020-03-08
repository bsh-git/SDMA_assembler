{-# LANGUAGE TupleSections #-}
--
--
module Main where

import Data.Char (toLower)
import Data.Word
import Data.Bits
import Data.Foldable (foldlM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Environment
import System.IO
import qualified System.Exit
import System.Console.GetOpt
import Sdma
import Sdma.Base
import Sdma.Cpp


data AssemblerOptions = AssemblerOptions {
      optOutputFormat :: OutputFormat
    , optLoadAddr :: Maybe Word16
    , optCppCommand :: String
    , optNoCpp :: Bool
    , optHelp :: Bool
    }
    deriving (Show, Eq)

data OutputFormat = C | Linux | Data
                  deriving (Show, Eq)

defaultOptions :: AssemblerOptions
defaultOptions = AssemblerOptions
    { optOutputFormat = C
    , optLoadAddr = Nothing
    , optCppCommand = "cpp --traditional"
    , optNoCpp = False
    , optHelp = False
    }


data CommandLineOpt =
      Format String
    | LoadAddr String
    | CppCommand String
    | NoCpp
    | Help
    deriving (Show)

options :: [OptDescr CommandLineOpt]
options =
    [ Option ['F'] ["format"] (ReqArg Format "FORMAT")
             "output format"
    , Option ['a'] ["addr"] (ReqArg LoadAddr "ADDR")
             "load address. relocatable code is generated if ommitted (not yet)"
    , Option ['C'] ["cpp-command"] (ReqArg CppCommand "COMMAND")
             "preprocessor command"
    , Option [] ["no-cpp"] (NoArg NoCpp)
             "do not pre-process"
    , Option ['?'] ["help"] (NoArg Help) "show help"
    ]
    
getOptions :: [String] -> Either String (AssemblerOptions, [String])
getOptions args = do
    (opts, a) <- case getOpt RequireOrder options args of
                   (f,a,[]) -> return (f,a)
                   (_,_,err) -> Left (concat err)

    o <- foldlM checkOption defaultOptions opts
    return (o, a)
  where
    checkOption :: AssemblerOptions -> CommandLineOpt -> Either String AssemblerOptions
    checkOption ao co =
        return =<< case co of
                     Format s -> case map toLower s of
                                 "c" -> return $ ao { optOutputFormat = C }
                                 "linux" -> return $ ao { optOutputFormat = Linux }
                                 "data" -> return $ ao { optOutputFormat = Data }
                                 _ -> Left $ "Unknown format : " ++ s
                     LoadAddr s -> return $ ao { optLoadAddr = Just (read s::Word16) }
                     CppCommand s -> return $ ao { optCppCommand = s }
                     NoCpp -> return $ ao { optNoCpp = True }
                     Help -> return $ ao { optHelp = True }
                     


main :: IO ()
main = do
    getArgs >>=
        (either optError main') . getOptions
  where
    main' (opts, args) =
        if optHelp opts
        then putStr =<< usage
        else
          case length args of
            0 -> B.hGetContents stdin >>= processFile  opts "(stdin)"
            1 -> B.readFile (head args) >>= processFile  opts (head args)
            _ -> die $ "too many argments" ++ (show args)

    usage = getProgName >>= \p -> return $ usageInfo (p ++ " [options] [input]") options
    optError msg = usage >>= \u -> die (msg ++ "\n" ++ u)

die :: String -> IO ()
die msg = do
    progname <- getProgName
    System.Exit.die (progname ++ ": " ++ msg)


processFile :: AssemblerOptions -> FilePath -> ByteString -> IO ()
processFile opts filename contents = do
    spi <- cppMarks filename contents

    either reportError outputCodes $ assembleFile filename contents Nothing spi

  where
      reportError e = do
          hPutStrLn stderr ("Syntax error(s) on " ++ filename)
          hPutStrLn stderr e
          die "abort."

      outputCodes codes =
          outputCodes' (map (\(h,l) -> shift (toWord32 h) 16 .|. toWord32 l)
                        (pairs 0 codes))
      outputCodes' = outputCodes'' (optOutputFormat opts)
      outputCodes'' Data codes = writeCodesAsData filename stdout codes
      outputCodes'' C codes = writeCodes filename stdout codes
      outputCodes'' Linux codes = writeCodes filename stdout codes

--
-- Local Variables:
-- coding: utf-8
-- End:
