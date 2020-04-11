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

loadTable table name = do
  content <- parseFile table
  return $ fromList name content

selectFromTable (x:xs) target ref = selectFromTable xs (fromTables target (colToTable x)) ref
  where
    colToTable (ColumnSimple name) = tableFromColumn name name ref
    colToTable (ColumnWithName name name') = tableFromColumn name name' ref
    colToTable (ColumnDistinct _ name) = tableFromColumn name name ref
    colToTable f@(AggregationColumn _ col _) = Agg.evaluate f (colToTable col)

selectFromTable _ target _ = target

convertTable t cols = if isEmpty t then tableFromCols cols else selectFromTable cols empty t

execute (Load file) = do
  table <- loadTable file file
  return $ show table

execute (Select isDistinct cols stmt) = do
  t <- tableExpression stmt empty
  let table = convertTable t cols
  return $ show $ tryDistinct isDistinct table

execute End = return "No input"

tableExpression (From table name stmt) (Table n header rows) = do
  table <- loadTable table name
  tableExpression stmt table

tableExpression (Where expr stmt) (Table n header rows) = tableExpression stmt (Table n header (exec rows))
  where
    exec (x:xs)
      | BoolInterpreter.evaluate expr (zip header x) = x : exec xs
      | otherwise = exec xs
    exec _ = []

tableExpression (OrderBy (x:xs) stmt) t@(Table n header rows) = tableExpression (OrderBy xs stmt) (Table n header (order x))
  where
    p (ColumnOrder n Ascending) = n
    order (ColumnOrder n Ascending) = sortOn (sort n) rows
    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
    sort n x = convert $ x !! columnIndex n t

tableExpression (OrderBy _ stmt) table = tableExpression stmt table

tableExpression End table = return table
