module Lexer where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.Ratio

type E a = Either String a
parseError :: [Token] -> E a
parseError _ = Left "Parse error"

data Value
  = ValueEmpty
  | ValueBool {vBool :: Bool}
  | ValueRat {vRat :: Rational}
  | ValueIdfr {vIdfr :: String}
  | ValueReasgn {vLHS :: String, vRHS :: Value}
  | ValueInit {vLHS :: String, vRHS :: Value}
  | ValueSeq Value Value
  | ValueSelection Value Value Value
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
  show ValueEmpty = ""
  show (ValueReasgn _ v) = show v
  show (ValueInit _ v) = show v
  show (ValueSeq _ v) = show v
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
  TokenOB |
  TokenCB |
  TokenSemi |
  TokenColon |
  TokenQM |
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
    '*' -> TokenTermOp op:lexer rest
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
lexer (':':cs) = TokenColon:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer ('{':cs) = TokenOB:lexer cs
lexer ('}':cs) = TokenCB:lexer cs
lexer (';':cs) = TokenSemi:lexer cs
lexer ('?':cs) = TokenQM:lexer cs
lexer _ = InvalidToken:[]
