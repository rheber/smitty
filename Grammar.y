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
  '::' {TokenElse}
  ':(' {TokenFalse}
  ':)' {TokenTrue}
  '\\' {TokenBS}
  '$' {TokenDollar}
  '(' {TokenOP}
  ')' {TokenCP}
  '{' {TokenOB}
  '}' {TokenCB}
  ';' {TokenSemi}
  '?' {TokenQM}
  '@' {TokenAt}
  '~' {TokenTilde}
  ',' {TokenComma}
%%

Stmts :: {Value}
  : LineStmt ';' Stmts {ValueSeq $1 $3}
  | While Stmts {ValueSeq $1 $2}
  | Selection Stmts {ValueSeq $1 $2}
  | LineStmt {$1}

LineStmt :: {Value}
  : {- empty -} {ValueEmpty}
  | '$' Disj {ValueReturn $2}
  | '$' {ValueReturn ValueEmpty}
  | Asgn {$1}
  | Delete {$1}

Selection :: {Value}
  : '?' Paren '{' Stmts '}' '::' SelectionTrailer {ValueSelection $2 $4 $7}
  | '?' Paren '{' Stmts '}' {ValueSelection $2 $4 ValueEmpty}

SelectionTrailer :: {Value}
  : '{' Stmts '}' {$2}
  | Paren '{' Stmts '}' {ValueSelection $1 $3 ValueEmpty}
  | Paren '{' Stmts '}' '::' SelectionTrailer {ValueSelection $1 $3 $6}

While :: {Value}
  : '@' '{' Stmts '}' Paren '{' Stmts '}' {ValueWhile $3 $5 $7}
  | '@' Paren '{' Stmts '}' {ValueWhile ValueEmpty $2 $4}
  | '@' '{' Stmts '}' Paren {ValueWhile $3 $5 ValueEmpty}

Asgn :: {Value}
  : identifier '::=' Disj {ValueReasgn $1 $3}
  | identifier ':=' Disj {ValueInit $1 $3}
  | Disj {$1}

Delete :: {Value}
  : '(' '~' identifier ')' {ValueDelete $3}

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
  : Term termOp Factor {ValueBinOp $2 $1 $3}
  | Factor {$1}

Factor :: {Value}
  : Atom Args {ValueFunction $1 $2}
  | Atom {$1}

{-
Args and Idfrs are parenthesised versions of the respective lists.
Args is used in Factor calls, Idfrs is used in Funcdefs.
-}

Args :: {[Value]}
  : '(' Arglist ')' {$2}

Arglist :: {[Value]}
  : {- empty -} {[ValueEmpty]}
  | Disj {[$1]}
  | Disj ',' Arglist {$1:$3}

Idfrs :: {[Value]}
  : '(' Idfrlist ')' {$2}

Idfrlist :: {[Value]}
  : {- empty -} {[ValueEmpty]}
  | Idfr {[$1]}
  | Idfr ',' Idfrlist {$1:$3}

Funcdef :: {Value}
  : '\\' Idfrs '{' Stmts '}' {ValueFuncdef $2 $4}

Idfr :: {Value}
  : identifier {ValueIdfr $1}

Atom :: {Value}
  : Bool {$1}
  | Rat {$1}
  | String {$1}
  | Idfr {$1}
  | Paren {$1}
  | Funcdef {$1}

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
