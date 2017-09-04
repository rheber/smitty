module Getopt where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess), exitWith)

data Opts = Opts {optEval :: Maybe String}

defaultOpts = Opts {optEval = Nothing}

showVersion :: Opts -> IO Opts
showVersion _ = putStrLn "smitty v0.1" >> exitWith ExitSuccess

options :: [OptDescr (Opts -> IO Opts)]
options =
  [Option "e" ["eval"] (ReqArg evalAction "CODE") "evaluate a code snippet",
   Option "v" ["version"] (NoArg showVersion) "show version number"]

evalAction :: String -> Opts -> IO Opts
evalAction arg opt = return opt {optEval = Just arg}

-- Simply parses the options.
runGetOpt :: [String] -> IO ([Opts -> IO Opts], [String])
runGetOpt args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
      where header = "Options:"

-- Get and parse args.
parseArgs :: IO [Opts -> IO Opts]
parseArgs = do
  args <- getArgs
  (actions, _) <- runGetOpt args
  return actions
