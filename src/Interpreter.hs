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

columns = map (\(Col col _ _) -> col)
names = map (\(Col _ name _) -> name)


tableFromCols cols = fromList (names cols : [columns cols])

execute (Load file) = do
  table <- loadTable file
  return $ show table

execute (Select cols stmt) = do
  let table = tableFromCols cols
  t <- tableExpression stmt table
  return $ show t

execute (Skip stm) = return $ "Cannot procces: " ++ stm

tableExpression :: Statement -> Table -> IO Table
tableExpression (From name stmt) (Table names columns) = do
  t <- loadTable name
  let table = select (map (fromMaybe "" . head) columns) names t
  tableExpression stmt table
tableExpression _ table = return table
