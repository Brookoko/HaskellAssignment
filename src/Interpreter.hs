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
    colToTable (ColumnSimple name) = tableFromColumn' name (toColumn name) ref
    colToTable (ColumnWithName name name') = tableFromColumn' name name' ref
    colToTable (ColumnDistinct _ name) = tableFromColumn' name (toColumn name) ref
    colToTable f@(AggregationColumn _ col _) = Agg.evaluate f (colToTable col)
    colToTable c@(CaseColumn cases def name) = caseToCol c ref

selectFromTable _ target _ = target

caseToCol (CaseColumn cases def n) (Table name header rows) = Table name [n] (mapCase rows)
  where
    mapCase (x:xs) = [matchCase x cases] : mapCase xs
    mapCase _ = []
    matchCase x (Branch expr value:ys)
      | BoolInterpreter.evaluate expr row = Just value
      | otherwise = matchCase x ys
      where row = Table name header [x]
    matchCase _ _ = def

convertTable table cols = if isEmpty table then tableFromCols cols else selectFromTable cols empty table

execute (Load file) = do
  table <- loadTable $ TableName file file
  return $ show table

execute (Select isDistinct cols stmt) = do
  t <- tableExpression stmt empty
  let table = convertTable t cols
  return $ show $ tryDistinct isDistinct table

execute End = return "No input"

tableExpression (From name stmt) table = do
  table <- loadTable name
  tableExpression stmt table

tableExpression (Where expr stmt) table = tableExpression stmt (evaluateBool expr table)

tableExpression (InnerJoin name expr stmt) table = do
  newTable <- loadTable name
  let joinTable = removeData $ fromTables table newTable
  let table' = inner table newTable joinTable expr
  tableExpression stmt table'

tableExpression (LeftJoin name expr stmt) table = do
  newTable <- loadTable name
  let joinTable = removeData $ fromTables table newTable
  let inn = inner table newTable joinTable expr
  let table' = leftWithout table newTable inn expr
  tableExpression stmt table'

tableExpression (RightJoin name expr stmt) table = do
  newTable <- loadTable name
  let joinTable = removeData $ fromTables table newTable
  let inn = inner table newTable joinTable expr
  let table' = rightWithout table newTable inn expr
  tableExpression stmt table'

tableExpression (FullJoin name expr stmt) table = do
  newTable <- loadTable name
  let joinTable = removeData $ fromTables table newTable
  let inn = inner table newTable joinTable expr
  let left = leftWithout table newTable inn expr
  let table' = rightWithout table newTable left expr
  tableExpression stmt table'

tableExpression (OrderBy (x:xs) stmt) t@(Table name header rows) = tableExpression (OrderBy xs stmt) (Table name header (order x))
  where
    order (ColumnOrder n Ascending) = sortOn (sort n) rows
    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
    sort n x = convert $ x !! toIndex n t

tableExpression (OrderBy _ stmt) table = tableExpression stmt table

tableExpression _ tables = return tables

leftWithout table newTable target expr = combine target (rows table) newTable
  where
    combine t (x:xs) newTable = combine (combineWith t x (rows newTable)) xs newTable
    combine t _ _ = t
    combineWith t@(Table name header _) x (y:ys)
      | BoolInterpreter.evaluate expr row = t
      | otherwise = combineWith t x ys
      where row = Table name header [x ++ y]
    combineWith t x _ = addRecord (x ++ emptyRow newTable) t

rightWithout table newTable target expr = combine target (rows newTable) table
  where
    combine t (x:xs) newTable = combine (combineWith t x (rows newTable)) xs newTable
    combine t _ _ = t
    combineWith t@(Table name header _) x (y:ys)
      | BoolInterpreter.evaluate expr row = t
      | otherwise = combineWith t x ys
      where row = Table name header [y ++ x]
    combineWith t x _ = addRecord (emptyRow table ++ x) t

inner table newTable target expr = combine target (rows table) newTable
  where
    combine t (x:xs) newTable = combine (combineWith t x (rows newTable)) xs newTable
    combine t _ _ = t
    combineWith t@(Table name header _) x (y:ys)
      | BoolInterpreter.evaluate expr row = combineWith (addRecord (x ++ y) t) x ys
      | otherwise = combineWith t x ys
      where row = Table name header [x ++ y]
    combineWith t _ _ = t

evaluateBool expr (Table name header rows) = Table name header (exec rows)
  where
    exec (x:xs)
      | BoolInterpreter.evaluate expr row = x : exec xs
      | otherwise = exec xs
      where
        row = Table name header [x]
    exec _ = []

row' = map (\(Table n header rows) -> Table n header [head rows])
rows' = map (\(Table n header rows) -> Table n header (tail rows))

packTables (Table n header row:xs) (Table _ _ rows:ys) = Table n header (row ++ rows) : packTables xs ys
packTables xs'@(x:xs) [] = xs'
packTables [] [] = []

removeData (Table n header rows) = Table n header []
