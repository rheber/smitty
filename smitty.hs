import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(parseStmt)
import Lexer (E, lexer)
import Env (Env(..), varInsert, varLookup, varMember, initialEnv)
import Value (Value(..), vIdfr, printValue)

localEnv :: Value -> Env -> [Value] -> Env
localEnv (ValueFuncdef params _) e@(Env m q) args
  | elem ValueEmpty params && not (elem ValueEmpty args) ||
    elem ValueEmpty args && not (elem ValueEmpty params) ||
    length params /= length args = e
  | otherwise =
    let m' = Map.fromList $ zip (vIdfr <$> params) (fmap ((flip eval) e) args)
    in Env (union m' m) q

eval :: Value -> Env -> Value
eval (ValueIdfr a) e = varLookup a e
eval (ValueFunction f xs) e = case eval f e of
  ValueFailure _ -> ValueFailure $ "Error: Undefined function " ++ vIdfr f
  ValueBuiltinExp op -> op $ (flip eval e) <$> xs
  func@_ -> evalFunc func (localEnv func e xs) xs
eval (ValueBinOp name b c) e = case varLookup name e of
  ValueFailure _ -> ValueFailure $ "Error: Undefined operator " ++ show name
  ValueBinExp op -> op (eval b e) $ eval c e
eval (ValueUnOp name b) e = case varLookup name e of
  ValueFailure _ -> ValueFailure $ "Error: Undefined operator " ++ show name
  ValueUnExp op -> op $ eval b e
eval (ValueSeq a v) e = let m = eval a e in case m of
  ValueFailure _ -> m
  ValueReturn r -> eval r e
  _ -> eval v (exec a e)
eval (ValueInit name v) e =
  if varMember name e
  then ValueFailure "Error: Variable already initialised"
  else eval v e
eval (ValueReasgn name v) e =
  if varMember name e
  then eval v e
  else ValueFailure "Error: Variable not initialised"
eval (ValueSelection cond a b) e = case eval cond e of
  ValueBool bl -> eval (if bl then a else b) e
  _ -> ValueFailure "Type error: Expected boolean"
eval w@(ValueWhile u cond v) e = eval (ValueSeq u $ case eval cond (exec u e) of
  ValueBool bl -> if bl then (ValueSeq v w) else ValueEmpty -- Ends empty anyway.
  _ -> ValueFailure "Type error: Expected boolean") e
eval (ValueReturn r) e = ValueReturn $ eval r e
eval v _ = v

stripReturns :: Value -> Value
stripReturns (ValueReturn r) = stripReturns r
stripReturns r = r

firstFailure :: [Value] -> Value
firstFailure [] = ValueEmpty
firstFailure ((ValueFailure f):xs) = ValueFailure f
firstFailure (_:xs) = firstFailure xs

evalFunc :: Value -> Env -> [Value] -> Value
evalFunc (ValueFuncdef params s) e args
  | elem ValueEmpty params && not (elem ValueEmpty args) ||
    elem ValueEmpty args && not (elem ValueEmpty params) ||
    length params /= length args = ValueFailure "Error: Wrong amount of arguments"
  | otherwise = let f = firstFailure (fmap ((flip eval) e) args) in
    if f == ValueEmpty then eval s e else f
evalFunc (ValueReturn v) e args = ValueReturn $ evalFunc v e args
evalFunc _ _ _ = ValueFailure "Error evaluating function"

exec :: Value -> Env -> Env
exec (ValueSeq a v) e = case (eval a e) of
  ValueFailure _ -> e
  ValueReturn r -> exec r (exec a e)
  _ -> exec v (exec a e)
exec (ValueInit name v) e = case v of
  ValueFailure _ -> e
  _ -> if varMember name e then e else varInsert name (eval v e) e
exec (ValueReasgn name v) e = case v of
  ValueFailure _ -> e
  _ -> if varMember name e then varInsert name (eval v e) e else e
exec (ValueSelection cond a b) e = case eval cond e of
  ValueBool bl -> exec (if bl then a else b) e
  _ -> e
exec w@(ValueWhile u cond v) e = exec (ValueSeq u $ case eval cond (exec u e) of
  ValueBool bl -> if bl then (ValueSeq v w) else ValueEmpty
  _ -> ValueEmpty) e
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
    let s = printValue $ stripReturns value
    putStr s
    if s /= "" then putStr "\n" else putStr ""
    repl env "" "smitty> "
  else repl e inputString "...> "

main :: IO ()
main = repl initialEnv "" "smitty> "
