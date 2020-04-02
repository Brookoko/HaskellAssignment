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

loadTable name = do
  content <- parseFile name
  return $ fromList content

columns = map (\(Col col _) -> col)
names = map (\(Col _ name) -> name)

tableFromCols cols = fromList (names cols : [columns cols])
selectFromTable cols = select (columns cols) (names cols)
dist process table = if process then distinct table else table

execute (Load file) = do
  table <- loadTable file
  return $ show table

execute (Select process cols stmt) = do
  t <- tableExpression stmt empty ""
  let table = if isEmpty t then tableFromCols cols else selectFromTable cols t
  return $ show $ dist process table

execute (Skip stm) = return $ "Cannot procces: " ++ stm

tableExpression (From name stmt) (Table names columns) _ = do
  table <- loadTable name
  tableExpression stmt table name
tableExpression _ table _ = return table
