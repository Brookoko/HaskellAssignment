module Interpreter
  (
    execute
  ) where

import Language
import Table
import Parser
import Data.List
import Control.Monad
import Data.Maybe
import Text.Read
import Data.Ord

data Comparable = CompInt Integer | CompString (Maybe String)

instance Eq Comparable where
  (CompInt x) == (CompInt y) = x == y
  (CompString x) == (CompString y) = x == y

instance Ord Comparable where
  compare (CompInt x) (CompInt y) = compare x y
  compare (CompString x) (CompString y) = compare x y
  compare (CompInt x) (CompString y) = LT
  compare (CompString x) (CompInt y) = GT

loadTable name = do
  content <- parseFile name
  return $ fromList content

columns = map (\(Col col _) -> col)
names = map (\(Col _ name) -> name)

tableFromCols cols = fromList (names cols : [columns cols])
selectFromTable cols = select (columns cols) (names cols)
dist process table = if process then distinct table else table

execute (Load file) = do
  table <- loadTable file
  return $ show table

execute (Select process cols stmt) = do
  t <- tableExpression stmt empty
  let table = if isEmpty t then tableFromCols cols else selectFromTable cols t
  return $ show $ dist process table

execute (Skip stm) = return $ "Cannot procces: " ++ stm

tableExpression (From name stmt) (Table header rows) = do
  table <- loadTable name
  tableExpression stmt table

tableExpression (Where expr stmt) (Table header rows) = tableExpression stmt (Table header (exec rows))
  where
    exec (x:xs)
      | evaluateBool expr (zip header x) = x : exec xs
      | otherwise = exec xs
    exec _ = []

tableExpression (OrderBy (x:xs) stmt) t@(Table header rows) = tableExpression (OrderBy xs stmt) (Table header (order x))
  where
    p (ColumnOrder n Ascending) = n
    order :: ColumnOrder -> [[Maybe String]]
    order (ColumnOrder n Ascending) = sortOn (sort n) rows
    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
    sort n x = convert $ x !! columnIndex n t
    convert x = maybe (CompString x) CompInt (readMaybe (fromMaybe "" x))

tableExpression (OrderBy _ stmt) table = tableExpression stmt table

tableExpression _ table = return table

evaluateBool (BoolConst bool) row = bool
evaluateBool (Not expr) row = not $ evaluateBool expr row

evaluateBool (BoolBinary And expr1 expr2) row = evaluateBool expr1 row && evaluateBool expr2 row
evaluateBool (BoolBinary Or expr1 expr2) row = evaluateBool expr1 row || evaluateBool expr2 row

evaluateBool op@(RelationBinary _ expr1 expr2) row = do
  let x = evaluateArithmetic expr1 row
  isJust x && (do
    let y = evaluateArithmetic expr2 row
    isJust y && evaluateSafely op (fromJust x) (fromJust y))
  where
    evaluateSafely (RelationBinary Equal _ _) x y = x == y
    evaluateSafely (RelationBinary NotEqual _ _) x y = x /= y
    evaluateSafely (RelationBinary Greater _ _) x y = x > y
    evaluateSafely (RelationBinary GreaterThan _ _) x y = x >= y
    evaluateSafely (RelationBinary Less _ _) x y = x < y
    evaluateSafely (RelationBinary LessThan _ _) x y = x <= y

evaluateBool (RelationTernary Between expr1 expr2 expr3) row =
  evaluateBool (RelationBinary GreaterThan expr1 expr2) row && evaluateBool (RelationBinary LessThan expr1 expr3) row

evaluateArithmetic (Var var) row = readMaybe $ fromMaybe "" variable
  where variable = snd $ fromMaybe ("", Just "") (find ((== var) . fst) row)
evaluateArithmetic (IntConst int) row = Just int
evaluateArithmetic (Neg expr) row = do
  let x = evaluateArithmetic expr row
  if isJust x then Just (-(fromJust x)) else Nothing

evaluateArithmetic op@(ArithmeticBinary _ expr1 expr2) row = do
  let x = evaluateArithmetic expr1 row
  if isNothing x then Nothing
  else do
    let y = evaluateArithmetic expr2 row
    if isNothing y then Nothing else Just $ evaluateSafely op (fromJust x) (fromJust y)
  where
    evaluateSafely (ArithmeticBinary Add _ _) x y = x + y
    evaluateSafely (ArithmeticBinary Subtract _ _) x y = x - y
    evaluateSafely (ArithmeticBinary Multiply _ _) x y= x * y
    evaluateSafely (ArithmeticBinary Divide _ _) x y = x `div` y
