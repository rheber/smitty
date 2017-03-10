import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(E(..), Value(..), lexer, parseStmt)

type Env = Map.Map String Value

varLookup :: String -> Env -> Value
varLookup name e = Map.findWithDefault
  (ValueFailure "Error: Variable used before initialised") name e

valuiseBool :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
valuiseBool f (ValueBool a) (ValueBool b) = ValueBool $ f a b
valuiseBool f _ _ = ValueFailure "Type error: Expected booleans"
valuiseRat :: (Rational -> Rational -> Rational) -> (Value -> Value -> Value)
valuiseRat f (ValueRat a) (ValueRat b) = ValueRat $ f a b
valuiseRat f _ _ = ValueFailure "Type error: Expected rationals"

-- Like valuiseRat but specially handle zero.
valuiseNonzero :: (Rational -> Rational -> Rational) -> (Value -> Value -> Value)
valuiseNonzero f _ (ValueRat 0) = ValueFailure "Error: Zero argument"
valuiseNonzero f (ValueRat a) (ValueRat b) = ValueRat $ f a b
valuiseNonzero f _ _ = ValueFailure "Type error: Expected rationals"

emptyEnv :: Env
emptyEnv = Map.empty

-- Environment loaded with standard operations.
initialEnv :: Env
initialEnv = Map.fromList [("||", ValueOper $ valuiseBool (||)),
  ("&&", ValueOper $ valuiseBool (&&)),
  ("+", ValueOper $ valuiseRat (+)),
  ("-", ValueOper $ valuiseRat (-)),
  ("*", ValueOper $ valuiseRat (*)),
  ("/", ValueOper $ valuiseNonzero (/))]

-- Potentially perform assignment.
handleAsgn :: Value -> Env -> Env
handleAsgn v e = case v of
  ValueReasgn name value -> case value of
    ValueFailure _ -> e
    _ -> if member name e then Map.insert name value e else e
  ValueInit name value -> case value of
    ValueFailure _ -> e
    _ -> if member name e then e else Map.insert name value e
  _ -> e

evalIfOk :: Env -> E Value -> Value
evalIfOk e v = case v of
  Ok a -> eval e a
  Failed s -> ValueFailure "Syntax error"

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
eval e (ValueOp a b c) = case varLookup a e of
  ValueFailure _ -> ValueFailure "Error: Undefined operator"
  op -> (vOper op) (eval e b) $ eval e c
eval _ v = v

repl :: Env -> IO ()
repl e = do
  putStr "smitty> "
  hFlush stdout
  tokens <- getLine
  let parsedStmt = parseStmt $ lexer tokens
  let value = evalIfOk e parsedStmt
  print value
  repl $ handleAsgn value e

main :: IO ()
main = repl initialEnv
