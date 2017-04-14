module Value where

import Data.Ratio (numerator, denominator)

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
