import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(dummyEnv, lexer, parseStmt)

repl :: IO ()
repl = do
  putStr "smitty> "
  hFlush stdout
  tokens <- getLine
  print $ (parseStmt $ lexer tokens) <*> dummyEnv
  repl

main :: IO ()
main = repl