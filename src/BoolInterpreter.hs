module BoolInterpreter where

import qualified ArithmeticInterpreter as Arithmetic
import Language
import Data.Maybe

evaluate (BoolConst bool) row = bool
evaluate (Not expr) row = not $ evaluate expr row

evaluate (BoolBinary And expr1 expr2) row = evaluate expr1 row && evaluate expr2 row
evaluate (BoolBinary Or expr1 expr2) row = evaluate expr1 row || evaluate expr2 row

evaluate op@(RelationBinary _ expr1 expr2) row = do
  let x = Arithmetic.evaluate expr1 row
  let y = Arithmetic.evaluate expr2 row
  isJust x && isJust y && evaluateSafely op (fromJust x) (fromJust y)
  where
    evaluateSafely (RelationBinary Equal _ _) x y = x == y
    evaluateSafely (RelationBinary NotEqual _ _) x y = x /= y
    evaluateSafely (RelationBinary Greater _ _) x y = x > y
    evaluateSafely (RelationBinary GreaterThan _ _) x y = x >= y
    evaluateSafely (RelationBinary Less _ _) x y = x < y
    evaluateSafely (RelationBinary LessThan _ _) x y = x <= y

evaluate (RelationTernary Between expr1 expr2 expr3) row =
  evaluate (RelationBinary GreaterThan expr1 expr2) row && evaluate (RelationBinary LessThan expr1 expr3) row
