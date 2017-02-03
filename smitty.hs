import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(E(..), Value(..), lexer, parseStmt)

type Env = Map.Map String Value

varLookup :: String -> Env -> Value
varLookup name e = Map.findWithDefault (ValueBool False) name e

valuiseBool :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
valuiseBool f (ValueBool a) (ValueBool b) = ValueBool $ f a b
valuiseBool f _ _ = ValueFailure "Type error: Expected booleans"
valuiseRat :: (Rational -> Rational -> Rational) -> (Value -> Value -> Value)
valuiseRat f (ValueRat a) (ValueRat b) = ValueRat $ f a b
valuiseRat f _ _ = ValueFailure "Type error: Expected rationals"

emptyEnv :: Env
emptyEnv = Map.empty

-- Environment loaded with standard operations.
initialEnv :: Env
initialEnv = Map.fromList [("||", ValueOper $ valuiseBool (||)),
  ("&&", ValueOper $ valuiseBool (&&)),
  ("+", ValueOper $ valuiseRat (+)),
  ("-", ValueOper $ valuiseRat (-)),
  ("*", ValueOper $ valuiseRat (*)),
  ("/", ValueOper $ valuiseRat (/))]

-- Potentially perform assignment.
handleAsgn :: Value -> Env -> Env
handleAsgn v e = case v of
  ValueAsgn name value -> Map.insert name value e
  _ -> e

evalIfOk :: Env -> E Value -> Value
evalIfOk e v = case v of
  Ok a -> eval e a
  Failed s -> ValueFailure "Syntax error"

eval :: Env -> Value -> Value
eval e (ValueAsgn a b) = ValueAsgn a $ eval e b
eval e (ValueIdfr a) = varLookup a e
eval e (ValueOp a b c) = (vOper $ varLookup a e) (eval e b) $ eval e c
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
