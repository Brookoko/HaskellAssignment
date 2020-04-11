module ColumnParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Monad
import Language

cols = sepBy1 tableColumn comma
tableColumn = aggregationColumn <|> column

aggregationColumn = do
  function <- aggregationFunction
  col <- parens distinctColumn
  name <- (reserved "as" >> name) <|> aggregateToString function col
  return $ AggregationColumn function col name

aggregateToString func (ColumnDistinct distinct name) = return $ show func ++ "(" ++ dist ++ name ++ ")"
  where dist = if distinct then "distinct " else ""

distinctColumn = do
  distinct <- isDistinct
  ColumnDistinct distinct <$> name

isDistinct = (reserved "distinct" >> return True) <|> return False

aggregationFunction =
  (reserved "count" >> return Count) <|>
  (reserved "min" >> return Min) <|>
  (reserved "max" >> return Max) <|>
  (reserved "avg" >> return Avg) <|>
  (reserved "sum" >> return Sum)

column = do
  col <- name
  colWithName col <|> simpleCol col

colWithName col = reserved "as" >> ColumnWithName col <$> name
simpleCol col = return $ ColumnSimple col

orderCols = sepBy1 orderCol comma

orderCol = do
  col <- name
  ColumnOrder col <$> orderType

orderType =
  (reserved "asc" >> return Ascending) <|>
  (reserved "desc" >> return Descending) <|>
  return Ascending
