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
import qualified AggregationInterpreter as Agg
import Comparable
import TableFunctions

loadTable (TableName table name) = do
  content <- parseFile table
  return $ fromList name content

selectFromTable (x:xs) target ref = selectFromTable xs (fromTables target (colToTable x)) ref
  where
    colToTable (ColumnSimple name) = tableFromColumn' name (toString name) ref
    colToTable (ColumnWithName name name') = tableFromColumn' name name' ref
    colToTable (ColumnDistinct _ name) = tableFromColumn' name (toString name) ref
    colToTable f@(AggregationColumn _ col _) = Agg.evaluate f (colToTable col)

selectFromTable _ target _ = target

convertTable tables cols = if null tables then tableFromCols cols else selectFromTable cols empty tables

execute (Load file) = do
  table <- loadTable $ TableName file file
  return $ show table

execute (Select isDistinct cols stmt) = do
  t <- tableExpression stmt []
  let table = convertTable t cols
  return $ show $ tryDistinct isDistinct table

execute End = return "No input"

tableExpression (From name stmt) tables = do
  table <- loadTable name
  tableExpression stmt (table:tables)

tableExpression (Where expr stmt) tables = do
  let tables' = evaluateBool expr tables
  tableExpression stmt tables'

--tableExpression (OrderBy (x:xs) stmt) tables = tableExpression (OrderBy xs stmt) (order x)
--  where
--    order (ColumnOrder n Ascending) = sortOn (sort n) rows
--    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
--    sort n x = convert $ x !! columnIndex (toColumn n) t

tableExpression (InnerJoin name expr stmt) tables = do
  table <- loadTable name
  let tables' = tables ++ [table]
  let t = repeatTables (len tables') tables'
  tableExpression stmt (evaluateBool expr t)
  where
    repeatTables rep (Table n header rows:xs) = Table n header (take rep (cycle rows')) : repeatTables rep xs
      where rows' = concatMap (replicate (count xs)) rows
    repeatTables _ _ = []
    count = foldr ((*) . length . rows) 1
    len (x:xs) = length (rows x) * count xs

tableExpression (OrderBy _ stmt) tables = tableExpression stmt tables

tableExpression _ tables = return tables

evaluateBool expr tables = if null tables' then map removeData tables else tables'
  where
    tables' = exec tables
    exec tables
      | hasNoData tables = []
      | BoolInterpreter.evaluate expr row = packTables row (exec rows)
      | otherwise = exec rows
      where
        row = row' tables
        rows = rows' tables

hasNoData = all (\(Table n header rows) -> null rows)
row' = map (\(Table n header rows) -> Table n header [head rows])
rows' = map (\(Table n header rows) -> Table n header (tail rows))

packTables (Table n header row:xs) (Table _ _ rows:ys) = Table n header (row ++ rows) : packTables xs ys
packTables xs'@(x:xs) [] = xs'
packTables [] [] = []

removeData (Table n header rows) = Table n header []
