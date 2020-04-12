module TableFunctions where

import Language
import Table
import Data.List
import Data.List.Split
import Data.Maybe

columns = map getColumn
names = map getName

toColumn (ColumnAndTable t n) = t ++ "." ++ n
toColumn (JustColumn n) = n

getColumn (ColumnWithName col _) = toColumn col
getColumn (ColumnSimple col) = toColumn col

getName (ColumnWithName _ name) = name
getName (ColumnSimple name) = toColumn name

tableFromCols cols = fromList "" (names cols : [columns cols])

distinct (Table n header rows) = Table n header (distinctRows rows)
distinctRows (x:xs) = x : distinctRows (filter (/=x) xs)
distinctRows _ = []

tryDistinct isDistinct table = if isDistinct then distinct table else table
tryDistinctRows isDistinct rows = if isDistinct then distinctRows rows else rows

tableFromColumn columnName alias t@(Table n header rows)
  | '*' `elem` column = t
  | hasColumn column t = Table n [alias] (map (\x -> [x !! i]) rows)
  | otherwise = empty
    where
      column = toColumn columnName
      i = columnIndex column t

hasColumn n (Table name header rows) = n `elem` header
columnIndex name (Table n header _) = fromMaybe (columnMissing name) (elemIndex name header)

tableFromColumn' columnName alias table@(Table name header rows) = from columnName
  where
    from (ColumnAndTable t n) = tableFromColumn columnName alias (allWithInfo t table)
    from (JustColumn n)
      | n == "*" = table
      | otherwise = withName n alias table

allWithInfo t (Table name header rows) =
  Table name (fromIndices indices header) (rowsIndices indices rows)
  where indices = findIndices ((== t) . takeWhile (/= '.')) header

withName n alias (Table name header rows)
  | null indices = columnMissing n
  | length indices > 1 = multipleEntry n
  | otherwise = Table name [alias] (rowsIndices indices rows)
  where indices = findIndices ((== n) . removeFromHeader) header

rowsIndices indices = transpose . fromIndices indices . transpose
fromIndices indices = from 0
  where
    from c (x:xs)
      | c `elem` indices = x : from (c+1) xs
      | otherwise = from (c+1) xs
    from _ [] = []

columnValue columnName table = head $ transpose $ rows table'
  where table' = tableFromColumn' columnName (toColumn columnName) table

columnMissing name = error $ "No column with name " ++ name
multipleEntry name = error $ "Multipler entry of " ++ name ++ ". Specify column name"
