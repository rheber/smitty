{
module Grammar where
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.Ratio
}

%name parseStmt
%tokentype {Token}
%error {parseError}
%monad {E} {thenE} {returnE}

%token
  identifier {TokenIdfr $$}
  rational {TokenRational $$}
  '::=' {TokenAssign}
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

Stmt :: {Env -> Value}
  : Asgn {\p -> $1}
  | Disj {\p -> ValueBool $1}
  | Sum {\p -> ValueRat $1}
  | Idfr {\p -> ValueIdfr $1}

Asgn :: {Value}
  : Idfr '::=' Disj {ValueBool $3}
  | Idfr '::=' Sum {ValueRat $3}

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

Idfr
  : identifier {ValIdfr $1}

Rat :: {ValRat}
  : rational {ValRat $1}
  | '(' Sum ')' {$2}

Bool :: {ValBool}
  : ':(' {ValBool False}
  | ':)' {ValBool True}
  | '(' Disj ')' {$2}

{
data E a = Ok a | Failed String
instance Functor E where
  fmap f u = case u of
    Ok a -> Ok $ f a
    Failed s -> Failed s
instance Applicative E where
  pure a = Ok a
  f <*> u = case u of
    Ok a -> case f of
      Ok g -> Ok $ g a
      Failed s -> Failed s
    Failed s -> Failed s
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

data Env = Env
dummyEnv = Ok Env

data Value = ValueBool ValBool | ValueRat ValRat | ValueIdfr ValIdfr
data ValIdfr = ValIdfr {vIdfr :: String}
data ValBool = ValBool {vBool :: Bool}
data ValRat =  ValRat {vRat :: Rational}

instance Show Value where
  show (ValueIdfr v) = show v
  show (ValueBool v) = show v
  show (ValueRat v) = show v
instance Show ValIdfr where
  show (ValIdfr v) = show v
instance Show ValBool where
  show (ValBool False) = ":("
  show (ValBool True) = ":)"
instance Show ValRat where
  show (ValRat r) = (show $ numerator r) ++ " / " ++ (show $ denominator r)

data Token =
  TokenIdfr String |
  TokenRational Rational |
  TokenAssign |
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

lexIdfr :: String -> [Token]
lexIdfr cs =
  case span isAlphaNum cs of
    (idfr, rest) -> TokenIdfr idfr:lexer rest

lexInteger :: String -> [Token]
lexInteger cs = TokenRational (toRational $ read num):lexer rest
  where (num,rest) = span isDigit cs

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isLetter c = lexIdfr (c:cs)
  | isDigit c = lexInteger (c:cs)
  | c == '#' = lexer $ tail $ dropWhile (\x -> x /= '#') cs
lexer (':':':':'=':cs) = TokenAssign:lexer cs
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
