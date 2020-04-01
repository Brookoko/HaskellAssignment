module Interpreter
  (
    execute
  ) where

import Language
import Table
import Parser
import Data.List;

loadTable name = do
  content <- parseFile name
  return $ fromList content

columns = map (\(Col col _) -> col)
names = map (\(Col _ name) -> name)

execute (Load file) = do
  table <- loadTable file
  return $ show table

execute (Select cols name stmt) = do
  t <- loadTable name
  let table = select (columns cols) (names cols) t
  return $ show table

execute (Skip stm) = return $ "Cannot procces: " ++ stm
