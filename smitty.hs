import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(E(..), Env, Value(ValueAsgn), lexer, parseStmt)

emptyEnv :: Env
emptyEnv = Map.empty

-- Potentially perform assignment.
handleAsgn :: E Value -> Env -> Env
handleAsgn v e = case v of
  Ok a -> case a of
    ValueAsgn name value -> Map.insert name value e
    _ -> e
  Failed s -> e

repl :: Env -> IO ()
repl e = do
  putStr "smitty> "
  hFlush stdout
  tokens <- getLine
  let value = (parseStmt $ lexer tokens) <*> (Ok e)
  print value
  repl $ handleAsgn value e

main :: IO ()
main = repl emptyEnv
