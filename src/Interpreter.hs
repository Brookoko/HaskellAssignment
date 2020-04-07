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
import qualified BoolInterpreter

data Comparable = CompInt Integer | CompString (Maybe String)

instance Eq Comparable where
  (CompInt x) == (CompInt y) = x == y
  (CompString x) == (CompString y) = x == y

instance Ord Comparable where
  compare (CompInt x) (CompInt y) = compare x y
  compare (CompString x) (CompString y) = compare x y
  compare (CompInt x) (CompString y) = LT
  compare (CompString x) (CompInt y) = GT

convert x = maybe (CompString x) CompInt (readMaybe (fromMaybe "" x))

toInt (CompInt x) = x
toInt _ = 0

loadTable name = do
  content <- parseFile name
  return $ fromList content

columns = map (\(ColumnName col _) -> col)
names = map (\(ColumnName _ name) -> name)

tableFromCols cols = fromList (names cols : [columns cols])

selectFromTable (x:xs) target ref = selectFromTable xs (fromTables target (colToTable x)) ref
  where
    colToTable (ColumnSimple name) = tableFromColumn name name ref
    colToTable (ColumnName name name') = tableFromColumn name name' ref
    colToTable (ColumnDistinct _ name) = tableFromColumn name name ref
    colToTable f@(AggregationColumn _ col _) = evaluateAggregation f (colToTable col)

selectFromTable _ target _ = target

evaluateAggregation f@(AggregationColumn _ (ColumnDistinct isDistinct name) h) t@(Table header rows) =
  Table [h] [[aggregate f (map head (tryDistinctRows isDistinct rows))]]
    where
      aggregate (AggregationColumn Count _ _) list = Just $ show $ length list
      aggregate (AggregationColumn Min _ _) list = minimumBy comp list
      aggregate (AggregationColumn Max _ _) list = maximumBy comp list
      aggregate (AggregationColumn Avg _ _) list = Just $ show $ average list
      aggregate (AggregationColumn Sum _ _) list = Just $ show $ sum $ convertList list
      comp x y = compare (convert x) (convert y)
      convertList = map convert
      sum = foldr ((+) . toInt) 0
      average list = realToFrac(sum (convertList list)) / realToFrac(length list)

distinct (Table header rows) = Table header (distinctRows rows)

distinctRows (x:xs) = x : distinctRows (filter (/=x) xs)
distinctRows _ = []

tryDistinct isDistinct table = if isDistinct then distinct table else table
tryDistinctRows isDistinct rows = if isDistinct then distinctRows rows else rows

execute (Load file) = do
  table <- loadTable file
  return $ show table

execute (Select isDistinct cols stmt) = do
  t <- tableExpression stmt empty
  let table = if isEmpty t then tableFromCols cols else selectFromTable cols empty t
  return $ show $ tryDistinct isDistinct table

execute (Skip stm) = return $ "Cannot procces: " ++ stm

tableExpression (From name stmt) (Table header rows) = do
  table <- loadTable name
  tableExpression stmt table

tableExpression (Where expr stmt) (Table header rows) = tableExpression stmt (Table header (exec rows))
  where
    exec (x:xs)
      | BoolInterpreter.evaluate expr (zip header x) = x : exec xs
      | otherwise = exec xs
    exec _ = []

tableExpression (OrderBy (x:xs) stmt) t@(Table header rows) = tableExpression (OrderBy xs stmt) (Table header (order x))
  where
    p (ColumnOrder n Ascending) = n
    order (ColumnOrder n Ascending) = sortOn (sort n) rows
    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
    sort n x = convert $ x !! columnIndex n t

tableExpression (OrderBy _ stmt) table = tableExpression stmt table

tableExpression _ table = return table
