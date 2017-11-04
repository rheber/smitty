module Env where

import Data.Map as Map
import Data.Sequence as Seq
import System.Exit (exitWith)

import Value

-- Variables and output queue.
data Env = Env {vars :: (Map.Map String Value), ioq :: (Seq.Seq QIO)} deriving Show

varLookup :: String -> Env -> Value
varLookup name (Env m _) = Map.findWithDefault
  (ValueFailure "Error: Variable used before initialised") name m

varMember :: String -> Env -> Bool
varMember name (Env m _) = Map.member name m

varInsert :: String -> Value -> Env -> Env
varInsert name v (Env m q) = Env (Map.insert name v m) q

varDelete :: String -> Env -> Env
varDelete name (Env m q) = Env (Map.delete name m) q

-- Add values to an env.
varUnion :: [String] -> [Value] -> Env -> Env
varUnion names vals (Env oldV oldQ) =
  Env (union (Map.fromList $ Prelude.zip names vals) oldV) oldQ

-- Put something into the IO queue.
qIO :: QIO -> Env -> Env
qIO i (Env m q) = Env m (q |> i)

-- Split the queue from the environment.
dq :: Env -> (Env, Seq.Seq QIO)
dq (Env m q) = ((Env m Seq.empty), q)

processq :: Seq.Seq QIO -> Env -> IO Env
processq xs e
  | Seq.null xs  = return e
  | otherwise = case (Seq.splitAt 1 xs) of
    (h, t) -> (process' (Seq.index h 0) e) >>= processq t

process' :: QIO -> Env -> IO Env
process' (Output s) e = printOutput s >> return e
process' (Input idfr) e = do
  str <- getLine
  return $ varInsert idfr (ValueString str) e
process' (Quit d) e = exitWith d >> return e

printOutput :: String -> IO()
printOutput "" = return ()
printOutput ('\\':'\\':xs) = putChar '\\' >> printOutput xs
printOutput ('\\':'"':xs) = putChar '"' >> printOutput xs
printOutput ('\\':'n':xs) = putChar '\n' >> printOutput xs
printOutput ('\\':'t':xs) = putChar '\t' >> printOutput xs
printOutput (x:xs) = putChar x >> printOutput xs

emptyEnv :: Env
emptyEnv = Env Map.empty Seq.empty

-- Environment loaded with standard operations.
initialEnv :: Env
initialEnv = Env (Map.fromList [
  ("||", ValueBinExp $ valuiseBool (||))
  ,("&&", ValueBinExp $ valuiseBool (&&))
  ,("==", ValueBinExp $ valuiseEq (==))
  ,("!=", ValueBinExp $ valuiseEq (/=))
  ,("<", ValueBinExp $ valuiseEq (<))
  ,("<=", ValueBinExp $ valuiseEq (<=))
  ,(">", ValueBinExp $ valuiseEq (>))
  ,(">=", ValueBinExp $ valuiseEq (>=))
  ,("+", ValueBinExp $ valuiseRat (+))
  ,("-", ValueBinExp $ valuiseRat (-))
  ,("*", ValueBinExp $ valuiseRat (*))
  ,("/", ValueBinExp $ valuiseNonzero (/))
  ,("!", ValueUnExp valuisedNeg)
  ,("approx", ValueBuiltinExp valuisedApprox)
  ,("exp", ValueBuiltinExp valuisedExp)
  ,("floor", ValueBuiltinExp valuisedFloor)
  ,("input", ValueBuiltinExp valuisedInput)
  ,("print", ValueBuiltinExp valuisedPrint)
  ,("quit", ValueBuiltinExp valuisedQuit)
  ]) Seq.empty
