import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Grammar(lexer, parseStmt)

repl :: IO ()
repl = do
  putStr "smitty> "
  hFlush stdout
  getLine >>= print . parseStmt . lexer
  repl

main :: IO ()
main = repl