{
module Grammar where
import Data.Char (isDigit, isSpace)
}

%name parseStmt
%tokentype {Token}
%error {parseError}

%token
  integer {TokenInteger $$}
  '+' {TokenPlus}
  '-' {TokenMinus}
  '*' {TokenStar}
  '/' {TokenFS}
  '(' {TokenOP}
  ')' {TokenCP}

%%

Stmt : Sum {Sum $1}

Sum
  : Sum '+' Term {Plus $1 $3}
  | Sum '-' Term {Minus $1 $3}
  | Term {Term $1}

Term
  : Term '*' Factor {Times $1 $3}
  | Term '/' Factor {Div $1 $3}
  | Factor {Factor $1}

Factor
  : integer {Integer $1}
  | '(' Sum ')' {Parens $2}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Stmt = Sum Sum deriving Show
data Sum =
  Plus Sum Term |
  Minus Sum Term |
  Term Term
  deriving Show
data Term =
  Times Term Factor |
  Div Term Factor |
  Factor Factor
  deriving Show
data Factor =
  Integer Integer |
  Parens Sum
  deriving Show

data Token =
  TokenInteger Integer |
  TokenPlus |
  TokenMinus |
  TokenStar |
  TokenFS |
  TokenOP |
  TokenCP |
  InvalidToken
  deriving Show

lexInteger :: String -> [Token]
lexInteger cs = TokenInteger (read num):lexer rest
  where (num,rest) = span isDigit cs

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isDigit c = lexInteger (c:cs)
lexer ('+':cs) = TokenPlus:lexer cs
lexer ('-':cs) = TokenMinus:lexer cs
lexer ('*':cs) = TokenStar:lexer cs
lexer ('/':cs) = TokenFS:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer _ = InvalidToken:[]
}
