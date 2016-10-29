{
module Grammar where
import Data.Char (isDigit, isSpace)
import Data.Ratio
}

%name parseStmt
%tokentype {Token}
%error {parseError}
%monad {E} {thenE} {returnE}

%token
  rational {TokenRational $$}
  '+' {TokenPlus}
  '-' {TokenMinus}
  '*' {TokenStar}
  '/' {TokenFS}
  '(' {TokenOP}
  ')' {TokenCP}

%%

Stmt : Sum {$1}

Sum
  : Sum '+' Term {ValRat $ (vRat $1) + (vRat $3)}
  | Sum '-' Term {ValRat $ (vRat $1) - (vRat $3)}
  | Term {$1}

Term
  : Term '*' Factor {ValRat $ (vRat $1) * (vRat $3)}
  | Term '/' Factor {ValRat $ (vRat $1) / (vRat $3)}
  | Factor {$1}

Factor
  : rational {ValRat $1}
  | '(' Sum ')' {$2}

{
data E a = Ok a | Failed String
instance Show a => Show (E a) where
  show (Ok a) = show a
  show (Failed e) = e

thenE m k = case m of
  Ok a -> k a
  Failed e -> Failed e
returnE a = Ok a
failE err = Failed err
catchE m k = case m of
  Ok a -> Ok a
  Failed e -> k e

parseError :: [Token] -> E a
parseError _ = failE "Parse error"

data Value =
  ValBool {vBool :: Bool} |
  ValRat {vRat :: Rational}

instance Show Value where
  show (ValRat r) = (show $ numerator r) ++ " / " ++ (show $ denominator r)

data Token =
  TokenRational Rational |
  TokenPlus |
  TokenMinus |
  TokenStar |
  TokenFS |
  TokenOP |
  TokenCP |
  InvalidToken
  deriving Show

lexInteger :: String -> [Token]
lexInteger cs = TokenRational (toRational $ read num):lexer rest
  where (num,rest) = span isDigit cs

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isDigit c = lexInteger (c:cs)
  | c == '#' = lexer $ tail $ dropWhile (\x -> x /= '#') cs
lexer ('+':cs) = TokenPlus:lexer cs
lexer ('-':cs) = TokenMinus:lexer cs
lexer ('*':cs) = TokenStar:lexer cs
lexer ('/':cs) = TokenFS:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer _ = InvalidToken:[]
}
