module Interpreter
  (
    execute
  ) where

import Language
import Table
import Parser
import Data.List;

createTable name = do
  content <- parseFile name
  return $ fromList content

execute (Load file) = do
  table <- createTable file
  return $ show table

execute (Select cols name stmt) = do
  t <- createTable name
  let table = select cols t
  return $ show table

execute (Skip stm) = return $ "Cannot procces: " ++ stm
