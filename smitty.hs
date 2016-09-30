import Grammar(lexer, parseStmt)

repl :: IO ()
repl = do
  getLine >>= print . parseStmt . lexer
  repl

main :: IO ()
main = repl