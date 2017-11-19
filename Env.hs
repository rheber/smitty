module Env where

import Data.Map as Map
import System.Exit (exitWith)

import Value

-- Variables in current scope, last string read and most recent evaluated value.
data Env = Env {vars :: (Map.Map String Value),
                inputValue :: Value,
                lastValue :: Value}
  deriving Show

clearValue :: Env -> Env
clearValue (Env m q _) = Env m q ValueEmpty

errValue :: Env -> String -> Env
errValue (Env m q _) f = Env m q $ ValueFailure f

varLookup :: String -> Env -> Value
varLookup name (Env m _ _) = Map.findWithDefault
  (ValueFailure "Error: Variable used before initialised") name m

varMember :: String -> Env -> Bool
varMember name (Env m _ _) = Map.member name m

varInsert :: String -> Value -> Env -> Env
varInsert name v (Env m q lv) = Env (Map.insert name v m) q lv

varDelete :: String -> Env -> Env
varDelete name (Env m q lv) = Env (Map.delete name m) q lv

-- Add values to an env.
varUnion :: [String] -> [Value] -> Env -> Env
varUnion names vals (Env oldVals q lv) =
  Env (union (Map.fromList $ Prelude.zip names vals) oldVals) q lv

execIO :: ActionIO -> Env -> IO Env
execIO (Output s) e = printOutput s >> return e {inputValue = ValueEmpty}
execIO Input e = getLine >>= \s -> return e {inputValue = ValueString s}
execIO (Quit d) e = exitWith d >> return e {inputValue = ValueEmpty}

printOutput :: String -> IO()
printOutput "" = return ()
printOutput ('\\':'\\':xs) = putChar '\\' >> printOutput xs
printOutput ('\\':'"':xs) = putChar '"' >> printOutput xs
printOutput ('\\':'n':xs) = putChar '\n' >> printOutput xs
printOutput ('\\':'t':xs) = putChar '\t' >> printOutput xs
printOutput (x:xs) = putChar x >> printOutput xs

emptyEnv :: Env
emptyEnv = Env Map.empty ValueEmpty ValueEmpty

-- Environment loaded with standard operations.
initialEnv :: Env
initialEnv = Env (Map.fromList [
  ("||", ValueBinDef $ valuiseBool (||))
  ,("&&", ValueBinDef $ valuiseBool (&&))
  ,("==", ValueBinDef $ valuiseEq (==))
  ,("!=", ValueBinDef $ valuiseEq (/=))
  ,("<", ValueBinDef $ valuiseEq (<))
  ,("<=", ValueBinDef $ valuiseEq (<=))
  ,(">", ValueBinDef $ valuiseEq (>))
  ,(">=", ValueBinDef $ valuiseEq (>=))
  ,("+", ValueBinDef $ valuiseRat (+))
  ,("-", ValueBinDef $ valuiseRat (-))
  ,("*", ValueBinDef $ valuiseRat (*))
  ,("/", ValueBinDef $ valuiseNonzero (/))
  ,("!", ValueUnDef valuisedNeg)
  ,("approx", ValueBuiltinDef valuisedApprox)
  ,("exp", ValueBuiltinDef valuisedExp)
  ,("floor", ValueBuiltinDef valuisedFloor)
  ,("input", ValueBuiltinDef valuisedInput)
  ,("print", ValueBuiltinDef valuisedPrint)
  ,("quit", ValueBuiltinDef valuisedQuit)
  ]) ValueEmpty ValueEmpty
