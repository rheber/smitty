module Getopt where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess), exitWith)

data Opts = Opts {optEval :: Maybe String, optFile :: Maybe String}

defaultOpts = Opts {optEval = Nothing, optFile = Nothing}

showVersion :: Opts -> IO Opts
showVersion _ = putStrLn "smitty v0.1" >> exitWith ExitSuccess

options :: [OptDescr (Opts -> IO Opts)]
options =
  [Option "e" ["eval"] (ReqArg evalAction "CODE") "evaluate a code snippet",
   Option "f" ["file"] (ReqArg fileAction "FILE") "evaluate a source file",
   Option "v" ["version"] (NoArg showVersion) "show version number"]

evalAction :: String -> Opts -> IO Opts
evalAction arg opt = return opt {optEval = Just arg}
fileAction :: String -> Opts -> IO Opts
fileAction arg opt = return opt {optFile = Just arg}

-- Parses the given arguments, returning options.
-- Non-options are thrown away.
runGetOpt :: [String] -> IO [Opts -> IO Opts]
runGetOpt args =
  case getOpt Permute options args of
    (o,_,[])   -> return o
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
      where header = "Options:"

-- Get command-line arguments, parse them and return options to perform actions on.
parseArgs :: IO [Opts -> IO Opts]
parseArgs = getArgs >>= runGetOpt
