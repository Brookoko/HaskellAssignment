module StringInterpreter where

import Language
import Data.Maybe
import Data.List
import TableFunctions
import Table

evaluate (VarString var) row = head $ columnValue var row
evaluate (StringConst const) row = Just const

