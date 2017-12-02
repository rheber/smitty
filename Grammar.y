{
module Grammar where

import Lexer (E, Token(..), parseError)
import Value (Value(..), Stmt(..))
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
  '.' {TokenDot}
%%

Stmts :: {Stmt}
  : LineStmt ';' Stmts {StmtSeq $1 $3}
  | While Stmts {StmtSeq $1 $2}
  | Selection Stmts {StmtSeq $1 $2}
  | LineStmt {StmtSeq $1 StmtSeqEnd}

LineStmt :: {Stmt}
  : {- empty -} {StmtEmpty}
  | '$' Disj {StmtReturn $2}
  | '$' {StmtReturn ValueEmpty}
  | Asgn {$1}
  | Delete {$1}

Selection :: {Stmt}
  : '?' Paren '{' Stmts '}' '::' SelectionTrailer {StmtSelect $2 $4 $7}
  | '?' Paren '{' Stmts '}' {StmtSelect $2 $4 StmtEmpty}

SelectionTrailer :: {Stmt}
  : '{' Stmts '}' {$2}
  | Paren '{' Stmts '}' {StmtSelect $1 $3 StmtEmpty}
  | Paren '{' Stmts '}' '::' SelectionTrailer {StmtSelect $1 $3 $6}

While :: {Stmt}
  : '@' '{' Stmts '}' Paren '{' Stmts '}' {StmtWhile $3 $5 $7}
  | '@' Paren '{' Stmts '}' {StmtWhile StmtEmpty $2 $4}
  | '@' '{' Stmts '}' Paren {StmtWhile $3 $5 StmtEmpty}

Asgn :: {Stmt}
  : identifier '::=' Disj {StmtReasgn $1 $3}
  | identifier ':=' Disj {StmtInit $1 $3}
  | Disj {StmtValue $1}

Delete :: {Stmt}
  : '(' '~' identifier ')' {StmtDelete $3}

Disj :: {Value}
  : Disj disjOp Conj {ValueBinCall $2 $1 $3}
  | Conj {$1}

Conj :: {Value}
  : Conj conjOp Cmp {ValueBinCall $2 $1 $3}
  | Cmp {$1}

Cmp :: {Value}
  : Cmp cmpOp Sum {ValueBinCall $2 $1 $3}
  | Sum {$1}

Sum :: {Value}
  : Sum sumOp Term {ValueBinCall $2 $1 $3}
  | Term {$1}

Term :: {Value}
  : Term termOp Factor {ValueBinCall $2 $1 $3}
  | Factor {$1}

Factor :: {Value}
  : Atom Args {ValueFuncCall $1 $2}
  | Atom '.' Atom Args {ValueMethCall $1 $3 $4}
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
  : '\\' Idfrs '{' Stmts '}' {ValueFuncDef $2 $4}

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
  | '(' UnOp Disj ')' {ValueUnCall $2 $3}

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
