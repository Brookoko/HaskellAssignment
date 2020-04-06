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

loadTable name = do
  content <- parseFile name
  return $ fromList content

columns = map (\(ColumnName col _) -> col)
names = map (\(ColumnName _ name) -> name)

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
      | BoolInterpreter.evaluate expr (zip header x) = x : exec xs
      | otherwise = exec xs
    exec _ = []

tableExpression (OrderBy (x:xs) stmt) t@(Table header rows) = tableExpression (OrderBy xs stmt) (Table header (order x))
  where
    p (ColumnOrder n Ascending) = n
    order (ColumnOrder n Ascending) = sortOn (sort n) rows
    order (ColumnOrder n Descending) = sortOn (Down . sort n) rows
    sort n x = convert $ x !! columnIndex n t
    convert x = maybe (CompString x) CompInt (readMaybe (fromMaybe "" x))

tableExpression (OrderBy _ stmt) table = tableExpression stmt table

tableExpression _ table = return table
