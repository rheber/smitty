{
module Grammar where
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.Map as Map
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
  : Asgn {$1}

Asgn :: {Env -> Value}
  : Idfr '::=' Disj {\p -> ValueAsgn $1 $ $3 p}
  | Disj {$1}

Disj :: {Env -> Value}
  : Disj '||' Conj {\p -> ValueBool $ (vBool $ $1 p) || (vBool $ $3 p)}
  | Conj {$1}

Conj :: {Env -> Value}
  : Conj '&&' Sum {\p -> ValueBool $ (vBool $ $1 p) && (vBool $ $3 p)}
  | Sum {$1}

Sum :: {Env -> Value}
  : Sum '+' Term {\p -> ValueRat $ (vRat $ $1 p) + (vRat $ $3 p)}
  | Sum '-' Term {\p -> ValueRat $ (vRat $ $1 p) - (vRat $ $3 p)}
  | Term {$1}

Term :: {Env -> Value}
  : Term '*' Atom {\p -> ValueRat $ (vRat $ $1 p) * (vRat $ $3 p)}
  | Term '/' Atom {\p -> ValueRat $ (vRat $ $1 p) / (vRat $ $3 p)}
  | Atom {$1}

IdfrVal :: {Env -> Value}
  : Idfr {\p -> varLookup $1 p}

Idfr :: {String}
  : identifier {$1}

Atom :: {Env -> Value}
  : Bool {\p -> $1}
  | Rat {\p -> $1}
  | IdfrVal {$1}
  | '(' Disj ')' {$2}

Rat :: {Value}
  : rational {ValueRat $1}

Bool :: {Value}
  : ':(' {ValueBool False}
  | ':)' {ValueBool True}

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

type Env = Map.Map String Value

varLookup :: String -> Env -> Value
varLookup name e = Map.findWithDefault (ValueBool False) name e

data Value
  = ValueBool {vBool :: Bool}
  | ValueRat {vRat :: Rational}
  | ValueAsgn String Value

instance Show Value where
  show (ValueAsgn _ v) = show v
  show (ValueBool False) = ":("
  show (ValueBool True) = ":)"
  show (ValueRat r) = (show $ numerator r) ++ " / " ++ (show $ denominator r)

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
