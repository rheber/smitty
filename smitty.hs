import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(parseStmt)
import Lexer (E, lexer)
import Value (Value(..),
  valuiseBool, valuiseEq, valuiseRat, valuiseNonzero, valuisedNeg)

type Env = Map.Map String Value

varLookup :: String -> Env -> Value
varLookup name e = Map.findWithDefault
  (ValueFailure "Error: Variable used before initialised") name e

emptyEnv :: Env
emptyEnv = Map.empty

-- Environment loaded with standard operations.
initialEnv :: Env
initialEnv = Map.fromList [("||", ValueBinExp $ valuiseBool (||))
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
  ,("!", ValueUnExp valuisedNeg)]

-- Potentially perform assignment.
handleAsgn :: Value -> Env -> Env
handleAsgn v e = case v of
  ValueReasgn name value -> case value of
    ValueFailure _ -> e
    _ -> if member name e then Map.insert name value e else e
  ValueInit name value -> case value of
    ValueFailure _ -> e
    _ -> if member name e then e else Map.insert name value e
  ValueSeq a b ->
    let e' = handleAsgn a e
    in handleAsgn b e'
  _ -> e

evalIfOk :: Env -> E Value -> Value
evalIfOk e v = case v of
  Right a -> eval e a
  Left s -> ValueFailure "Syntax error"

eval :: Env -> Value -> Value
eval e (ValueReasgn a b) =
  if member a e
  then ValueReasgn a $ eval e b
  else ValueFailure "Error: Variable not initialised"
eval e (ValueInit a b) =
  if member a e
  then ValueFailure "Error: Variable already initialised"
  else ValueInit a $ eval e b
eval e (ValueIdfr a) = varLookup a e
eval e (ValueBinOp a b c) = case varLookup a e of
  ValueFailure _ -> ValueFailure "Error: Undefined operator"
  op -> (vBin op) (eval e b) $ eval e c
eval e (ValueUnOp a b) = case varLookup a e of
  ValueFailure _ -> ValueFailure "Error: Undefined operator"
  op -> (vUn op) (eval e b)
eval e (ValueSeq a b) = let
  e' = handleAsgn a e
  in ValueSeq (eval e a) (eval e' b)
eval e (ValueSelection a b c) = case eval e a of
  ValueBool bl -> if bl then (eval e b) else (eval e c)
  _ -> ValueFailure "Type error: Expected boolean"
eval _ v = v

repl :: Env -> IO ()
repl e = do
  putStr "smitty> "
  hFlush stdout
  tokens <- getLine
  let parsedStmt = parseStmt $ lexer tokens
  let value = evalIfOk e parsedStmt
  let s = show value
  putStr s
  if s /= "" then putStr "\n" else putStr ""
  repl $ handleAsgn value e

main :: IO ()
main = repl initialEnv
