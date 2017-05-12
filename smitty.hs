import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(parseStmt)
import Lexer (E, lexer)
import Value (Value(..), printValue,
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

eval :: Value -> Env -> Value
eval (ValueIdfr a) e = varLookup a e
eval (ValueBinOp name b c) e = case varLookup name e of
  ValueFailure _ -> ValueFailure "Error: Undefined operator"
  op -> (vBin op) (eval b e) $ eval c e
eval (ValueUnOp name b) e = case varLookup name e of
  ValueFailure _ -> ValueFailure "Error: Undefined operator"
  op -> (vUn op) (eval b e)
eval (ValueSeq a v) e = eval v (exec a e)
eval (ValueInit name v) e =
  if member name e
  then ValueFailure "Error: Variable already initialised"
  else eval v e
eval (ValueReasgn name v) e =
  if member name e
  then eval v e
  else ValueFailure "Error: Variable not initialised"
eval (ValueSelection cond a b) e = case eval cond e of
  ValueBool bl -> eval (if bl then a else b) e
  _ -> ValueFailure "Type error: Expected boolean"
eval w@(ValueSimpleWhile cond v) e = case eval cond e of
  ValueBool bl -> eval (if bl then (ValueSeq v w) else ValueEmpty) e
  _ -> ValueFailure "Type error: Expected boolean"
eval v _ = v

exec :: Value -> Env -> Env
exec (ValueSeq a v) e = exec v (exec a e)
exec (ValueInit name v) e = case v of
  ValueFailure _ -> e
  _ -> if member name e then e else Map.insert name (eval v e) e
exec (ValueReasgn name v) e = case v of
  ValueFailure _ -> e
  _ -> if member name e then Map.insert name (eval v e) e else e
exec (ValueSelection cond a b) e = case eval cond e of
  ValueBool bl -> exec (if bl then a else b) e
  _ -> e
exec w@(ValueSimpleWhile cond v) e = case eval cond e of
  ValueBool bl -> exec (if bl then (ValueSeq v w) else ValueEmpty) e
  _ -> e
exec _ e = e

run :: E Value -> Env -> (Value, Env)
run v e = case v of
  Right a -> (eval a e, exec a e)
  Left s -> (ValueFailure s, e)

repl :: Env -> String -> String -> IO ()
repl e oldInput prompt = do
  putStr prompt
  hFlush stdout
  newInput <- getLine
  let inputString = oldInput ++ newInput
  let parsedStmt = parseStmt $ lexer inputString
  if parsedStmt /= Left "Syntax error: Unexpected end of input"
  then do
    let (value, env) = run parsedStmt e
    let s = printValue value
    putStr s
    if s /= "" then putStr "\n" else putStr ""
    repl env "" "smitty> "
  else repl e inputString "...> "

main :: IO ()
main = repl initialEnv "" "smitty> "
