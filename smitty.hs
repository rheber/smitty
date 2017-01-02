import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(E(Ok), Env, Value(ValueAsgn), lexer, parseStmt, unwrapE)

emptyEnv :: Env
emptyEnv = Map.empty

-- Potentially perform assignment.
handleAsgn :: Value -> Env -> Env
handleAsgn v e = case v of
  ValueAsgn name value -> Map.insert name value e
  _ -> e

repl :: Env -> IO ()
repl e = do
  putStr "smitty> "
  hFlush stdout
  tokens <- getLine
  let value = (parseStmt $ lexer tokens) <*> (Ok e)
  print value
  repl $ handleAsgn (unwrapE value) e

main :: IO ()
main = repl emptyEnv
