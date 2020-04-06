module ArithmeticInterpreter where

import Language
import Data.Maybe
import Text.Read
import Data.List

evaluate (Var var) row = readMaybe $ fromMaybe "" variable
  where variable = snd $ fromMaybe ("", Just "") (find ((== var) . fst) row)
evaluate (IntConst int) row = Just int
evaluate (Neg expr) row = do
  let x = evaluate expr row
  if isJust x then Just (-(fromJust x)) else Nothing

evaluate op@(ArithmeticBinary _ expr1 expr2) row = do
  let x = evaluate expr1 row
  if isNothing x then Nothing
  else do
    let y = evaluate expr2 row
    if isNothing y then Nothing else Just $ evaluateSafely op (fromJust x) (fromJust y)
  where
    evaluateSafely (ArithmeticBinary Add _ _) x y = x + y
    evaluateSafely (ArithmeticBinary Subtract _ _) x y = x - y
    evaluateSafely (ArithmeticBinary Multiply _ _) x y= x * y
    evaluateSafely (ArithmeticBinary Divide _ _) x y = x `div` y
