import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(parseStmt)
import Lexer (E, lexer)
import Env (Env(..), varInsert, varDelete, varLookup, varMember, varUnion,
  clearValue, errValue, execIO, initialEnv)
import Value (Value(..), Stmt(..), vIdfr, printValue)
import Getopt (Opts(..), defaultOpts, parseArgs)

evalMap :: [Value] -> Env -> IO [Value]
evalMap vs e = sequence $ (flip eval e) <$> vs

wrongArgs params args =
  let p = elem ValueEmpty params
      a = elem ValueEmpty args
  in a && not p || p && not a || length params /= length args

localEnv :: Value -> Env -> [Value] -> IO Env
localEnv (ValueFuncDef params _) e args
  | wrongArgs params args = return $ errValue e "Error: Wrong amount of arguments"
  | otherwise = do
      argvals <- evalMap args e
      return $ varUnion (vIdfr <$> params) argvals e

firstFailure :: [Value] -> Value
firstFailure [] = ValueEmpty
firstFailure ((ValueFailure f):xs) = ValueFailure f
firstFailure (_:xs) = firstFailure xs

evalFunc :: Value -> Env -> [Value] -> IO Value
evalFunc (ValueFuncDef params s) e args
  | wrongArgs params args = return $
      ValueFailure "Error: Wrong amount of arguments"
  | otherwise = do
    xs <- evalMap args e
    let f = firstFailure xs in
      if f == ValueEmpty then do
        m <- lastValue <$> exec s e
        eval m e
      else return f
evalFunc (ValueReturn v) e args = ValueReturn <$> evalFunc v e args
evalFunc _ _ _ = return $ ValueFailure "Error evaluating function"

eval :: Value -> Env -> IO Value
eval (ValueIdfr a) e = return $ varLookup a e
eval (ValueBinCall name b c) e = case varLookup name e of
  ValueFailure _ -> return $ ValueFailure $
    "Error: Undefined operator " ++ show name
  ValueBinDef op -> op <$> (eval b e) <*> eval c e
eval (ValueUnCall name b) e = case varLookup name e of
  ValueFailure _ -> return $ ValueFailure $
    "Error: Undefined operator " ++ show name
  ValueUnDef op -> op <$> eval b e
eval (ValueMethCall x f [ValueEmpty]) e = eval (ValueFuncCall f [x]) e
eval (ValueMethCall x f xs) e = eval (ValueFuncCall f (x:xs)) e
eval (ValueFuncCall f xs) e = do
  v <- eval f e
  args <- evalMap xs e
  case v of
    ValueFailure _ -> return $ ValueFailure $
      "Error: Undefined function " ++ vIdfr f
    ValueBuiltinDef op -> eval (op args) e
    func@_ -> do
      loc <- localEnv func e args
      evalFunc func loc args
eval (ValueIO i) e = execIO i e >>= \s -> return $ inputValue s
eval (ValueReturn v) e = eval v e
eval v _ = return v

exec :: Stmt -> Env -> IO Env
exec StmtEmpty e = return $ clearValue e
exec (StmtValue v) e = do
  v' <- eval v e
  return e {lastValue = v'}
exec (StmtFailure v) e = return $ errValue e v
exec (StmtReturn v) e = do
  v' <- eval v e
  return e {lastValue = ValueReturn v'}
exec (StmtInit name v) e = case v of
  f@(ValueFailure _) -> return e {lastValue = f}
  _ -> if varMember name e
       then return $ errValue e "Error: Variable already initialised"
       else do
         v' <- eval v e
         return $ varInsert name v' $ clearValue e
exec (StmtReasgn name v) e = case v of
  f@(ValueFailure _) -> return e {lastValue = f}
  _ -> if varMember name e
       then do
         v' <- eval v e
         return $ varInsert name v' $ clearValue e
       else return $ errValue e "Error: Variable not initialised"
exec (StmtDelete name) e = return $ varDelete name $ clearValue e
exec (StmtSeq a b) e = do
  e' <- exec a e
  case lastValue e' of
    ValueFailure _ -> return e'
    ValueReturn _ -> return e'
    _ -> exec b e'
exec (StmtSelect cond a b) e = do
    v' <- eval cond e
    case v' of
      ValueBool bl -> exec (if bl then a else b) $ clearValue e
      _ -> return $ errValue e "Type error: Expected boolean"
exec w@(StmtWhile u cond v) e = do
  e' <- exec u e
  v' <- eval cond e'
  exec (StmtSeq u $ case v' of
    ValueBool bl -> if bl then (StmtSeq v w) else StmtEmpty
    _ -> StmtFailure "Type error: Expected boolean") e'
exec _ e = return e

run :: E Stmt -> Env -> IO Env
run v e = either (return . (errValue e)) (flip exec e) v

repl :: Env -> String -> String -> IO ()
repl e oldInput prompt = do
  putStr prompt
  hFlush stdout
  newInput <- getLine
  let inputString = oldInput ++ newInput
  let parsedStmt = parseStmt $ lexer inputString
  if parsedStmt /= Left "Syntax error: Unexpected end of input"
  then do
    e' <- run parsedStmt e
    let s = printValue $ lastValue e'
    putStr s
    if s /= "" then putStr "\n" else putStr ""
    repl e' "" "smitty> "
  else repl e inputString "...> "

-- Evaluate chunk of code with no code following.
-- Contrast with repl where environment has to be handed forwards.
evalAll :: String -> IO ()
evalAll code = do
  env <- run (parseStmt $ lexer code) initialEnv
  case lastValue env of
    ValueFailure s -> putStr s
    _ -> return ()

-- Prioritises files over one-liners.
main :: IO ()
main = do
  opts <- parseArgs >>= (foldl (>>=) (return defaultOpts))
  maybe (maybe (repl initialEnv "" "smitty> ") evalAll $ optEval opts)
    (\x -> readFile x >>= evalAll) $ optFile opts
