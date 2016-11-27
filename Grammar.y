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
  ':(' {TokenFalse}
  ':)' {TokenTrue}
  '||' {TokenOr}
  '&&' {TokenAnd}
  '+' {TokenPlus}
  '-' {TokenMinus}
  '*' {TokenStar}
  '/' {TokenFS}
  '(' {TokenOP}
  ')' {TokenCP}

%%

Stmt :: {String}
  : Disj {show $1}
  | Sum {show $1}

Disj :: {ValBool}
  : Disj '||' Conj {ValBool $ (vBool $1) || (vBool $3)}
  | Conj {$1}

Conj :: {ValBool}
  : Conj '&&' Bool {ValBool $ (vBool $1) && (vBool $3)}
  | Bool {$1}

Sum :: {ValRat}
  : Sum '+' Term {ValRat $ (vRat $1) + (vRat $3)}
  | Sum '-' Term {ValRat $ (vRat $1) - (vRat $3)}
  | Term {$1}

Term :: {ValRat}
  : Term '*' Rat {ValRat $ (vRat $1) * (vRat $3)}
  | Term '/' Rat {ValRat $ (vRat $1) / (vRat $3)}
  | Rat {$1}

Rat :: {ValRat}
  : rational {ValRat $1}
  | '(' Sum ')' {$2}

Bool :: {ValBool}
  : ':(' {ValBool False}
  | ':)' {ValBool True}
  | '(' Disj ')' {$2}

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

data ValBool = ValBool {vBool :: Bool}
data ValRat =  ValRat {vRat :: Rational}

instance Show ValBool where
  show (ValBool False) = ":("
  show (ValBool True) = ":)"
instance Show ValRat where
  show (ValRat r) = (show $ numerator r) ++ " / " ++ (show $ denominator r)

data Token =
  TokenRational Rational |
  TokenFalse |
  TokenTrue |
  TokenOr |
  TokenAnd |
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
lexer (':':'(':cs) = TokenFalse:lexer cs
lexer (':':')':cs) = TokenTrue:lexer cs
lexer ('|':'|':cs) = TokenOr:lexer cs
lexer ('&':'&':cs) = TokenAnd:lexer cs
lexer ('+':cs) = TokenPlus:lexer cs
lexer ('-':cs) = TokenMinus:lexer cs
lexer ('*':cs) = TokenStar:lexer cs
lexer ('/':cs) = TokenFS:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer _ = InvalidToken:[]
}
