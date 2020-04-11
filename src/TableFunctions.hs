module TableFunctions where

import Language
import Table
import Data.List
import Data.Maybe

columns = map getColumn
names = map getName

toColumn (ColumnAndTable t n) = n
toColumn (JustColumn n) = n

toString (ColumnAndTable t n) = t ++ "." ++ n
toString (JustColumn n) = n

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
  | column == "*" = t
  | hasColumn column t = Table n [alias] (map (\x -> [x !! i]) rows)
  | otherwise = empty
    where
      column = toColumn columnName
      i = columnIndex column t

hasColumn n (Table name header rows) = n `elem` header
columnIndex name (Table n header _) = fromMaybe (columnMissing name) (elemIndex name header)
columnMissing name = error $ "No column with name " ++ name

tableFromColumn' columnName alias = from columnName
  where
    from (ColumnAndTable t n) (x:xs)
      | t == Table.name x && n == "*" = x
      | t == Table.name x && hasColumn n x = tableFromColumn columnName alias x
      | otherwise = from columnName xs
    from (JustColumn n) xs'@(x:xs)
      | n == "*" = foldl fromTables empty xs'
      | n /= "*" && hasColumn n x && any (hasColumn n) xs =
        error $ "Multipler entry of " ++ toString columnName ++ ". Specify column name"
      | hasColumn n x = tableFromColumn columnName alias x
      | otherwise = from columnName xs
    from _ [] = columnMissing (toString columnName)

columnValue column t@(Table name header rows) = map (!! i) rows
  where i = columnIndex column t
