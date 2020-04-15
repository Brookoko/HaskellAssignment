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
    colToTable :: Column -> Table
    colToTable (ColumnSimple name) = tableFromColumn' name (toColumn name) ref
    colToTable (ColumnWithName name name') = tableFromColumn' name name' ref
    colToTable (ColumnDistinct _ name) = tableFromColumn' name (toColumn name) ref
    colToTable f@(AggregationColumn _ col@(ColumnDistinct _ n) _) = Agg.evaluate f (colToTable col) (toIndex n ref)
    colToTable c@(CaseColumn cases def name) = caseToCol c ref

selectFromTable _ target _ = target

caseToCol (CaseColumn cases def n) (Table name header rows groups) = Table name [n] (mapCase rows) groups
  where
    mapCase (x:xs) = [matchCase x cases] : mapCase xs
    mapCase _ = []
    matchCase x (Branch expr value:ys)
      | BoolInterpreter.evaluate expr row = Just value
      | otherwise = matchCase x ys
      where row = Table name header [x] groups
    matchCase _ _ = def

convertTable table cols = if isEmpty table then tableFromCols cols else selectFromTable cols empty table

execute (Load file) = do
  table <- loadTable $ TableName file file
  return $ show table

execute (Select isDistinct cols stmt) = do
  t <- tableExpression stmt empty
  let (AggregationColumn _ (ColumnDistinct _ n) _) = cols !! 1
  print n
  print $ toIndex n t
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

tableExpression (OrderBy cols stmt) t = tableExpression stmt (sortTable cols t)

tableExpression (Group cols stmt) t@(Table name header rows groups) = do
  let order = map (`ColumnOrder` Ascending) (reverse cols)
  let table' = sortTable order t
  let grouped = groupBy (equal indices) rows
  tableExpression stmt (Table name header (map head grouped) grouped)
  where
    indices = findIndices ((`elem` map toColumn cols) . removeFromHeader) header
    equal :: [Int] -> [Maybe String] -> [Maybe String] -> Bool
    equal (z:zs) x y
      | x !! z == y !! z = equal zs x y
      | otherwise = False
    equal _ x y = True

tableExpression (Having expr stmt) table = tableExpression stmt (evaluateBool expr table)

tableExpression _ tables = return tables

leftWithout table newTable target expr = combine target (rows table) newTable
  where
    combine t (x:xs) newTable = combine (combineWith t x (rows newTable)) xs newTable
    combine t _ _ = t
    combineWith t@(Table name header _ _) x (y:ys)
      | BoolInterpreter.evaluate expr row = t
      | otherwise = combineWith t x ys
      where row = Table name header [x ++ y] (groups table)
    combineWith t x _ = addRecord (x ++ emptyRow newTable) t

rightWithout table newTable target expr = combine target (rows newTable) table
  where
    combine t (x:xs) newTable = combine (combineWith t x (rows newTable)) xs newTable
    combine t _ _ = t
    combineWith t@(Table name header _ groups) x (y:ys)
      | BoolInterpreter.evaluate expr row = t
      | otherwise = combineWith t x ys
      where row = Table name header [y ++ x] groups
    combineWith t x _ = addRecord (emptyRow table ++ x) t

inner table newTable target expr = combine target (rows table) newTable
  where
    combine t (x:xs) newTable = combine (combineWith t x (rows newTable)) xs newTable
    combine t _ _ = t
    combineWith t@(Table name header _ _) x (y:ys)
      | BoolInterpreter.evaluate expr row = combineWith (addRecord (x ++ y) t) x ys
      | otherwise = combineWith t x ys
      where row = Table name header [x ++ y] (groups table)
    combineWith t _ _ = t

evaluateBool expr (Table name header rows groups) = Table name header (exec rows) groups
  where
    exec (x:xs)
      | BoolInterpreter.evaluate expr row = x : exec xs
      | otherwise = exec xs
      where
        row = Table name header [x] groups
    exec _ = []

sortTable (x:xs) t@(Table name header rows groups) = sortTable xs (Table name header (order x) (sortGroup x))
  where
    sortGroup (ColumnOrder n Ascending) = sortOn (sorting n . head) groups
    sortGroup (ColumnOrder n Descending) = sortOn (Down. sorting n . head) groups
    order (ColumnOrder n Ascending) = sortOn (sorting n) rows
    order (ColumnOrder n Descending) = sortOn (Down . sorting n) rows
    sorting n x = convert $ x !! toIndex n t
sortTable _ t = t

removeData (Table n header rows groups) = Table n header [] groups
