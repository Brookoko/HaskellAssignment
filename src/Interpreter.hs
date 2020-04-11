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
    colToTable (ColumnSimple name) = tableFromColumn' name (toString name) ref
    colToTable (ColumnWithName name name') = tableFromColumn' name name' ref
    colToTable (ColumnDistinct _ name) = tableFromColumn' name (toString name) ref
    colToTable f@(AggregationColumn _ col _) = Agg.evaluate f (colToTable col)

selectFromTable _ target _ = target

convertTable tables cols = if null tables then tableFromCols cols else selectFromTable cols empty tables

execute (Load file) = do
  table <- loadTable file file
  return $ show table

execute (Select isDistinct cols stmt) = do
  t <- tableExpression stmt []
  let table = convertTable t cols
  return $ show $ tryDistinct isDistinct table

execute End = return "No input"

tableExpression :: Statement -> [Table] -> IO [Table]
tableExpression (From table name stmt) tables = do
  table <- loadTable table name
  tableExpression stmt (table:tables)

tableExpression (Where expr stmt) tables = tableExpression stmt (exec tables)
  where
    exec :: [Table] -> [Table]
    exec tables
      | isEmpty = []
      | BoolInterpreter.evaluate expr row = pack row (exec rows)
      | otherwise = exec rows
      where
        isEmpty = all (\(Table n header rows) -> null rows) tables
        row = map (\(Table n header rows) -> Table n header [head rows]) tables
        rows = map (\(Table n header rows) -> Table n header (tail rows)) tables
    pack (Table n header row:xs) (Table _ _ rows:ys) = Table n header (row ++ rows) : pack xs ys
    pack xs'@(x:xs) [] = xs'
    pack [] [] = []

--tableExpression (OrderBy (x:xs) stmt) tables = tableExpression (OrderBy xs stmt) (order x)
--  where
--    order (ColumnOrder n Ascending) = sortOn (sort n) rows
--    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
--    sort n x = convert $ x !! columnIndex (toColumn n) t

tableExpression (OrderBy _ stmt) tables = tableExpression stmt tables

tableExpression _ tables = return tables
