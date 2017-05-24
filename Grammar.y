{
module Grammar where

import Lexer (E, Token(..), parseError)
import Value (Value(..))
}

%name parseStmt
%tokentype {Token}
%error {parseError}
%monad {E} {(>>=)} {return}

%token
  identifier {TokenIdfr $$}
  rational {TokenRational $$}
  string {TokenString $$}
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
  '{' {TokenOB}
  '}' {TokenCB}
  ';' {TokenSemi}
  '?' {TokenQM}
  '@' {TokenAt}
%%

Stmts :: {Value}
  : Stmt ';' Stmts {ValueSeq $1 $3}
  | Stmt {$1}

Stmt :: {Value}
  : {- empty -} {ValueEmpty}
  | Asgn {$1}
  | Selection {$1}
  | While {$1}

Selection :: {Value}
  : '?' Paren '{' Stmts '}' '{' Stmts '}' {ValueSelection $2 $4 $7}

While :: {Value}
  : '@' '{' Stmts '}' Paren '{' Stmts '}' {ValueWhile $3 $5 $7}

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
  | String {$1}
  | Idfr {$1}
  | Paren {$1}

Paren :: {Value}
  : '(' Disj ')' {$2}
  | '(' UnOp Disj ')' {ValueUnOp $2 $3}

UnOp :: {String}
  : disjOp {$1}
  | conjOp {$1}
  | cmpOp {$1}
  | sumOp {$1}
  | termOp {$1}

Rat :: {Value}
  : rational {ValueRat $1}

String :: {Value}
  : string {ValueString $1}

Bool :: {Value}
  : ':(' {ValueBool False}
  | ':)' {ValueBool True}
