module TableFunctions where

import Language
import Table
import Data.List
import Data.Maybe

columns = map getColumn
names = map getName

getColumn (ColumnWithName col _) = col
getColumn (ColumnSimple col) = col

getName (ColumnWithName _ name) = name
getName (ColumnSimple name) = name

tableFromCols cols = fromList "" (names cols : [columns cols])

distinct (Table n header rows) = Table n header (distinctRows rows)
distinctRows (x:xs) = x : distinctRows (filter (/=x) xs)
distinctRows _ = []

tryDistinct isDistinct table = if isDistinct then distinct table else table
tryDistinctRows isDistinct rows = if isDistinct then distinctRows rows else rows

tableFromColumn name h t@(Table n header rows)
  | name == "*" = t
  | otherwise = Table n [h] (map (\x -> [x !! i]) rows)
    where i = columnIndex name t

columnIndex name (Table n header _) = fromMaybe err (elemIndex name header)
  where err = error ("No column with name: " ++ name)
