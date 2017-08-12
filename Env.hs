module Env where

import Data.Map as Map

import Value (Value(..), valuisedApprox, valuisedExp,
  valuiseBool, valuiseEq, valuiseRat, valuiseNonzero, valuisedNeg)

data Env = Env (Map.Map String Value) ()

varLookup :: String -> Env -> Value
varLookup name (Env m _) = Map.findWithDefault
  (ValueFailure "Error: Variable used before initialised") name m

varMember :: String -> Env -> Bool
varMember name (Env m _) = Map.member name m

varInsert :: String -> Value -> Env -> Env
varInsert name v (Env m q) = Env (Map.insert name v m) q

emptyEnv :: Env
emptyEnv = Env Map.empty ()

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
  ]) ()
