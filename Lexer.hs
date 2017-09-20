module Lexer where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)

type E a = Either String a
parseError :: [Token] -> E a
parseError [] = Left "Syntax error: Unexpected end of input"
parseError (InvalidToken c:_) = Left $ "Syntax error: Unexpected character " ++ [c]
parseError (MissingQuoteToken:_) = Left $ "Syntax error: Missing closing \""
parseError _ = Left "Syntax error"

data Token
  = TokenIdfr String
  | TokenRational Rational
  | TokenString String
  | TokenReassign
  | TokenInit
  | TokenFalse
  | TokenTrue
  | TokenDisjOp String
  | TokenConjOp String
  | TokenCmpOp String
  | TokenSumOp String
  | TokenTermOp String
  | TokenOP
  | TokenCP
  | TokenOB
  | TokenCB
  | TokenSemi
  | TokenQM
  | TokenAt
  | TokenBS
  | TokenDollar
  | TokenTilde
  | TokenComma
  | InvalidToken Char
  | MissingQuoteToken
  deriving Show

lexString :: String -> [Token]
lexString cs = lexStr "" cs

lexStr s ('"':cs) = (TokenString $ reverse s):lexer cs
lexStr s ('\\':c:cs) = lexStr (c:'\\':s) cs
lexStr s (c:cs) = lexStr (c:s) cs
lexStr s "" = [MissingQuoteToken]

lexIdfr :: String -> [Token]
lexIdfr cs = case span isAlphaNum cs of
  (idfr, rest) -> TokenIdfr idfr:lexer rest

lexInteger :: String -> [Token]
lexInteger cs = TokenRational (toRational $ read num):lexer rest
  where (num,rest) = span isDigit cs

lexOp :: String -> [Token]
lexOp s = case span isOpchar s of
  (op@(c:_), rest) -> case c of
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
lexer [] = [] -- End of input.
lexer ('#':cs) = lexer $ tail $ dropWhile (\x -> x /= '#') cs -- Comments.
lexer (':':':':'=':cs) = TokenReassign:lexer cs
lexer (':':'=':cs) = TokenInit:lexer cs
lexer (':':'(':cs) = TokenFalse:lexer cs
lexer (':':')':cs) = TokenTrue:lexer cs
lexer ('\\':cs) = TokenBS:lexer cs
lexer ('$':cs) = TokenDollar:lexer cs
lexer ('(':cs) = TokenOP:lexer cs
lexer (')':cs) = TokenCP:lexer cs
lexer ('{':cs) = TokenOB:lexer cs
lexer ('}':cs) = TokenCB:lexer cs
lexer (';':cs) = TokenSemi:lexer cs
lexer ('?':cs) = TokenQM:lexer cs
lexer ('@':cs) = TokenAt:lexer cs
lexer ('~':cs) = TokenTilde:lexer cs
lexer (',':cs) = TokenComma:lexer cs
lexer ('"':cs) = lexString cs
lexer (c:cs)
  | isSpace c = lexer cs
  | isLetter c = lexIdfr (c:cs)
  | isDigit c = lexInteger (c:cs)
  | isOpchar c = lexOp (c:cs)
  | otherwise = [InvalidToken c]
