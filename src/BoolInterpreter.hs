module BoolInterpreter (evaluate) where

import qualified ArithmeticInterpreter as Arithmetic
import Language
import Data.Maybe
import qualified StringInterpreter as String
import qualified AggregationInterpreter as Agg;

evaluate (BoolConst bool) row = bool
evaluate (Not expr) row = not $ evaluate expr row

evaluate (BoolBinary And expr1 expr2) row = evaluate expr1 row && evaluate expr2 row
evaluate (BoolBinary Or expr1 expr2) row = evaluate expr1 row || evaluate expr2 row

evaluate (RelationBinary op expr1 expr2) row = do
  let x = Arithmetic.evaluate expr1 row
  let y = Arithmetic.evaluate expr2 row
  isJust x && isJust y && evaluateSafely op (fromJust x) (fromJust y)

evaluate (RelationTernary Between expr1 expr2 expr3) row =
  evaluate (RelationBinary GreaterThan expr1 expr2) row && evaluate (RelationBinary LessThan expr1 expr3) row

evaluate (RelationBinaryString op expr1 expr2) row = do
  let x = String.evaluate expr1 row
  let y = String.evaluate expr2 row
  isJust x && isJust y && evaluateSafely op (fromJust x) (fromJust y)
  where
    evaluateSafely Equal x y = x == y
    evaluateSafely NotEqual x y = x /= y
    evaluateSafely _ x y = False

evaluate (RelationAggregation op expr1 expr2) row = do
  let x = evaluateAggregation expr1 row
  let y = evaluateAggregation expr2 row
  evaluateSafely op x y

evaluateAggregation (VarInt int) row = realToFrac int
evaluateAggregation (AggregationExpression f name) row = Agg.aggregateHaving f name row

evaluateSafely Equal x y = x == y
evaluateSafely NotEqual x y = x /= y
evaluateSafely Greater x y = x > y
evaluateSafely GreaterThan x y = x >= y
evaluateSafely Less x y = x < y
evaluateSafely LessThan x y = x <= y
