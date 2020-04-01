module Interpreter
  (
    execute
  ) where

import Language
import Table
import Parser
import qualified Data.Map.Strict as Map

execute (Load file) = do
  content <- parseFile file
  return $ fromList content

execute _ = return Empty
