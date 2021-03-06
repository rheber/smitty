module Value where

import Data.Ratio (numerator, denominator)
import System.Exit (ExitCode(..))

instance Show (a -> b) where
  showsPrec _ _ = showString "<function>"

data Value
  = ValueEmpty
  | ValueBool Bool
  | ValueRat Rational
  | ValueString String
  | ValueIdfr String
  | ValueBinCall String Value Value
  | ValueBinDef (Value -> Value -> Value)
  | ValueUnCall String Value
  | ValueUnDef (Value -> Value)
  | ValueMethCall Value Value [Value]
  | ValueFuncCall Value [Value]
  | ValueFuncDef [Value] Stmt
  | ValueBuiltinDef ([Value] -> Value)
  | ValueIO ActionIO
  | ValueReturn Value
  | ValueFailure String
  deriving Show

data Stmt
  = StmtEmpty
  | StmtSeqEnd
  | StmtValue Value
  | StmtInit String Value
  | StmtReasgn String Value
  | StmtDelete String
  | StmtSeq Stmt Stmt
  | StmtSelect Value Stmt Stmt
  | StmtWhile Stmt Value Stmt
  | StmtReturn Value
  | StmtFailure String
  deriving Show

data ActionIO
  = Output String
  | Input
  | Quit ExitCode
  deriving Show

vIdfr :: Value -> String
vIdfr (ValueIdfr s) = s
vIdfr x = show x

instance Eq Stmt where
  StmtEmpty == StmtEmpty = True
  _ == _ = False
instance Eq Value where
  (ValueBool a) == (ValueBool b) = a == b
  (ValueRat a) == (ValueRat b) = a == b
  (ValueString a) == (ValueString b) = a == b
  ValueEmpty == ValueEmpty = True
  _ == _ = False
instance Ord Value where
  (ValueRat a) <= (ValueRat b) = a <= b
  (ValueString a) <= (ValueString b) = a <= b
  u <= v = u == v
  (ValueRat a) >= (ValueRat b) = a >= b
  (ValueString a) >= (ValueString b) = a >= b
  u >= v = u == v
  (ValueRat a) < (ValueRat b) = a < b
  (ValueString a) < (ValueString b) = a < b
  _ < _ = False
  (ValueRat a) > (ValueRat b) = a > b
  (ValueString a) > (ValueString b) = a > b
  _ > _ = False

-- What the REPL prints.
printValue :: Value -> String
printValue ValueEmpty = ""
printValue (ValueIO _) = ""
printValue (ValueBool False) = ":("
printValue (ValueBool True) = ":)"
printValue (ValueRat r) = (show $ numerator r) ++ " / " ++ (show $ denominator r)
printValue (ValueString s) = "\"" ++ s ++ "\"" -- Escapes in s not interpreted.
printValue (ValueReturn r) = printValue r
printValue (ValueFailure s) = s
printValue v = show v

valuiseBool :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
valuiseBool f (ValueBool a) (ValueBool b) = ValueBool $ f a b
valuiseBool f _ _ = ValueFailure "Type error: Expected booleans"
valuiseRat :: (Rational -> Rational -> Rational) -> (Value -> Value -> Value)
valuiseRat f (ValueRat a) (ValueRat b) = ValueRat $ f a b
valuiseRat f _ _ = ValueFailure "Type error: Expected rationals"

valuiseEq :: (a -> b -> Bool) -> (a -> b -> Value)
valuiseEq f v u = ValueBool $ f v u

-- Like valuiseRat but specially handle zero.
valuiseNonzero :: (Rational -> Rational -> Rational) -> (Value -> Value -> Value)
valuiseNonzero f _ (ValueRat 0) = ValueFailure "Error: Zero argument"
valuiseNonzero f (ValueRat a) (ValueRat b) = ValueRat $ f a b
valuiseNonzero f _ _ = ValueFailure "Type error: Expected rationals"

valuisedNeg :: Value -> Value
valuisedNeg (ValueBool a) = (ValueBool $ not a)
valuisedNeg _ = ValueFailure "Type error: Expected boolean"

valuisedApprox :: [Value] -> Value
valuisedApprox [ValueRat a] = ValueString $ show $ fromRational a
valuisedApprox _ = ValueFailure "Error: approx expected 1 rational argument"

valuisedExp :: [Value] -> Value
valuisedExp [ValueRat a, ValueRat b] =
  ValueRat $ toRational $ (fromRational a) ** (fromRational b)
valuisedExp _ = ValueFailure "Error: exp expected 2 rational arguments"

valuisedFloor :: [Value] -> Value
valuisedFloor [ValueRat a] = ValueRat $ toRational $ floor a
valuisedFloor _ = ValueFailure "Error: floor expected 1 rational argument"

valuisedPrint :: [Value] -> Value
valuisedPrint [ValueString s] = ValueIO $ Output s
valuisedPrint _ = ValueFailure "Error: print expected 1 string argument"

valuisedInput :: [Value] -> Value
valuisedInput [ValueEmpty] = ValueIO Input
valuisedInput _ = ValueFailure "Error: input expected 0 arguments"

valuisedQuit :: [Value] -> Value
valuisedQuit [ValueRat a]
  | a == 0 = ValueIO $ Quit ExitSuccess
  | (denominator a) == 1
    = ValueIO $ Quit $ ExitFailure $ fromIntegral $ numerator a
valuisedQuit _ = ValueFailure "Error: print expected 1 integer argument"
