module Env where

import Data.Map as Map
import Data.Sequence as Seq

import Value (Value(..), valuisedApprox, valuisedExp, valuisedPrint,
  valuiseBool, valuiseEq, valuiseRat, valuiseNonzero, valuisedNeg)

-- Variables and output queue.
data Env = Env (Map.Map String Value) (Seq.Seq Value) deriving Show

varLookup :: String -> Env -> Value
varLookup name (Env m _) = Map.findWithDefault
  (ValueFailure "Error: Variable used before initialised") name m

varMember :: String -> Env -> Bool
varMember name (Env m _) = Map.member name m

varInsert :: String -> Value -> Env -> Env
varInsert name v (Env m q) = Env (Map.insert name v m) q

qOutput :: Value -> Env -> Env
qOutput v (Env m q) = Env m (q |> v)

dq :: Env -> (Env, Seq.Seq Value)
dq (Env m q) = ((Env m Seq.empty), q)

printq :: Seq.Seq Value -> IO ()
printq q = mapM_ (\(ValueOutput s) -> printOutput s) q

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
  ,("print", ValueBuiltinExp valuisedPrint)
  ]) Seq.empty
