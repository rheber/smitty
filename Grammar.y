{
module Grammar where
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.Ratio
}

%name parseStmt
%tokentype {Token}
%error {parseError}
%monad {E} {(>>=)} {return}

%token
  identifier {TokenIdfr $$}
  rational {TokenRational $$}
  disjOp {TokenDisjOp $$}
  conjOp {TokenConjOp $$}
  cmpOp {TokenCmpOp $$}
  sumOp {TokenSumOp $$}
  termOp {TokenTermOp $$}
  '::=' {TokenReassign}
  ':=' {TokenInit}
  ':(' {TokenFalse}
  ':)' {TokenTrue}
  '(' {TokenOP}
  ')' {TokenCP}

%%

Stmt :: {Value}
  : Asgn {$1}

Asgn :: {Value}
  : identifier '::=' Disj {ValueReasgn $1 $3}
  | identifier ':=' Disj {ValueInit $1 $3}
  | Disj {$1}

Disj :: {Value}
  : Disj disjOp Conj {ValueBinOp $2 $1 $3}
  | Conj {$1}

Conj :: {Value}
  : Conj conjOp Cmp {ValueBinOp $2 $1 $3}
  | Cmp {$1}

Cmp :: {Value}
  : Cmp cmpOp Sum {ValueBinOp $2 $1 $3}
  | Sum {$1}

Sum :: {Value}
  : Sum sumOp Term {ValueBinOp $2 $1 $3}
  | Term {$1}

Term :: {Value}
  : Term termOp Atom {ValueBinOp $2 $1 $3}
  | Atom {$1}

Idfr :: {Value}
  : identifier {ValueIdfr $1}

Atom :: {Value}
  : Bool {$1}
  | Rat {$1}
  | Idfr {$1}
  | '(' Disj ')' {$2}
  | '(' UnOp Disj ')' {ValueUnOp $2 $3}

UnOp :: {String}
  : disjOp {$1}
  | conjOp {$1}
  | cmpOp {$1}
  | sumOp {$1}
  | termOp {$1}

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
instance Monad E where
  return = Ok
  m >>= k = case m of
    Ok a -> k a
    Failed e -> Failed e
instance Show a => Show (E a) where
  show (Ok a) = show a
  show (Failed e) = e
parseError :: [Token] -> E a
parseError _ = Failed "Parse error"

data Value
  = ValueBool {vBool :: Bool}
  | ValueRat {vRat :: Rational}
  | ValueIdfr {vIdfr :: String}
  | ValueReasgn {vLHS :: String, vRHS :: Value}
  | ValueInit {vLHS :: String, vRHS :: Value}
  | ValueBinOp String Value Value
  | ValueBinExp {vBin :: (Value -> Value -> Value)}
  | ValueUnOp String Value
  | ValueUnExp {vUn :: (Value -> Value)}
  | ValueFailure String

instance Eq Value where
  (ValueBool a) == (ValueBool b) = a == b
  (ValueRat a) == (ValueRat b) = a == b
  _ == _ = False
instance Ord Value where
  (ValueRat a) <= (ValueRat b) = a <= b
  u <= v = u == v
  (ValueRat a) >= (ValueRat b) = a >= b
  u >= v = u == v
  (ValueRat a) < (ValueRat b) = a < b
  _ < _ = False
  (ValueRat a) > (ValueRat b) = a > b
  _ > _ = False

instance Show Value where
  show (ValueReasgn _ v) = show v
  show (ValueInit _ v) = show v
  show (ValueBool False) = ":("
  show (ValueBool True) = ":)"
  show (ValueRat r) = (show $ numerator r) ++ " / " ++ (show $ denominator r)
  show (ValueFailure s) = s

data Token =
  TokenIdfr String |
  TokenRational Rational |
  TokenReassign |
  TokenInit |
  TokenFalse |
  TokenTrue |
  TokenDisjOp String |
  TokenConjOp String |
  TokenCmpOp String |
  TokenSumOp String |
  TokenTermOp String |
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

lexOp :: String -> [Token]
lexOp s@(c:_) = case span isOpchar s of
  (op, rest) -> case c of
    '|' -> TokenDisjOp op:lexer rest
    '&' -> TokenConjOp op:lexer rest
    '=' -> TokenCmpOp op:lexer rest
    '!' -> TokenCmpOp op:lexer rest
    '<' -> TokenCmpOp op:lexer rest
    '>' -> TokenCmpOp op:lexer rest
    '+' -> TokenSumOp op:lexer rest
    '-' -> TokenSumOp op:lexer rest
    '/' -> TokenTermOp op:lexer rest
    '%' -> TokenTermOp op:lexer rest

opchars = ".|&=!<>+-*/%"
isOpchar :: Char -> Bool
isOpchar c = elem c opchars

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isLetter c = lexIdfr (c:cs)
  | isDigit c = lexInteger (c:cs)
  | isOpchar c = lexOp (c:cs)
  | c == '#' = lexer $ tail $ dropWhile (\x -> x /= '#') cs
lexer (':':':':'=':cs) = TokenReassign:lexer cs
lexer (':':'=':cs) = TokenInit:lexer cs
lexer (':':'(':cs) = TokenFalse:lexer cs
lexer (':':')':cs) = TokenTrue:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer _ = InvalidToken:[]
}
