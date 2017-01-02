import Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(E(Ok), Env, lexer, parseStmt)

emptyEnv :: Env
emptyEnv = Map.empty

repl :: Env -> IO ()
repl e = do
  putStr "smitty> "
  hFlush stdout
  tokens <- getLine
  print $ (parseStmt $ lexer tokens) <*> (Ok e)
  repl e

main :: IO ()
main = repl emptyEnv
