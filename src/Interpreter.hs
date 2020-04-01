module Interpreter
  (
    execute
  ) where

import Language
import Table
import Parser
import Data.List
import Control.Monad

loadTable name = do
  content <- parseFile name
  return $ fromList content

columns = map (\(Col col _ _) -> col)
names = map (\(Col _ name _) -> name)

tableFromCols cols (From name _) = do
  table <- loadTable name
  return $ select (columns cols) (names cols) table
tableFromCols cols _  = return $ fromList (names cols : [columns cols])

execute (Load file) = do
  table <- loadTable file
  return $ show table

execute (Select cols stmt) = do
  table <- tableFromCols cols stmt
  let s = statement stmt
  return $ show table
    where
      statement (From _ s) = s
      statement _ = stmt

execute (Skip stm) = return $ "Cannot procces: " ++ stm
