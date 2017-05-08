module Lexer where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)

type E a = Either String a
parseError :: [Token] -> E a
parseError [] = Left "Syntax error: Unexpected end of input"
parseError (InvalidToken c:_) = Left $ "Syntax error: Unexpected character " ++ [c]
parseError _ = Left "Syntax error"

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
  TokenQM |
  TokenAt |
  InvalidToken Char
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
lexer (':':':':'=':cs) = TokenReassign:lexer cs
lexer (':':'=':cs) = TokenInit:lexer cs
lexer (':':'(':cs) = TokenFalse:lexer cs
lexer (':':')':cs) = TokenTrue:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer ('{':cs) = TokenOB:lexer cs
lexer ('}':cs) = TokenCB:lexer cs
lexer (';':cs) = TokenSemi:lexer cs
lexer ('?':cs) = TokenQM:lexer cs
lexer ('@':cs) = TokenAt:lexer cs
lexer (c:cs)
  | isSpace c = lexer cs
  | isLetter c = lexIdfr (c:cs)
  | isDigit c = lexInteger (c:cs)
  | isOpchar c = lexOp (c:cs)
  | c == '#' = lexer $ tail $ dropWhile (\x -> x /= '#') cs
  | otherwise = [InvalidToken c]
