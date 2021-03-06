module LanguageParser
  (
    parseString
  ) where

import System.IO
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Monad
import Language
import ExpressionParser
import ColumnParser

whileParser = whiteSpace >> statement

statement = parens statement <|> seqStatement
seqStatement = do
  list <- sepBy1 statement' semi
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Statement
statement' =
  load <|>
  select <|>
  from <|>
  innerJoin <|>
  fullJoin <|>
  leftJoin <|>
  rightJoin <|>
  order <|>
  where' <|>
  group <|>
  having <|>
  end <|>
  skip

load = do
  reserved "load"
  file <- parens name
  return $ Load file

select = do
  reserved "select"
  distinct <- isDistinct
  cols <- cols
  Select distinct cols <$> statement'

from = do
  reserved "from"
  table <- tableName
  From table <$> statement'

tableName = do
  table <- name
  n <- try name <|> return table
  return $ TableName table n

where' = do
  reserved "where"
  expr <- boolExpression
  Where expr <$> statement'

order = do
  reserved "order"
  reserved "by"
  cols <- orderCols
  OrderBy (reverse cols) <$> statement'

innerJoin = do
  reserved "inner"
  joinStmt InnerJoin

fullJoin = do
  reserved "full"
  reserved "outer"
  joinStmt FullJoin

leftJoin = do
  reserved "left"
  joinStmt LeftJoin

rightJoin = do
  reserved "right"
  joinStmt RightJoin

joinStmt const = do
  reserved "join"
  table <- tableName
  reserved "on"
  expr <- boolExpression
  const table expr <$> statement'

group = do
  reserved "group"
  reserved "by"
  cols <- sepBy1 columnName comma
  Group cols <$> statement'

having = do
  reserved "having"
  expr <- boolExpressionHaving
  Having expr <$> statement'

end = do
  eof
  return End

skip = do
  n <- name
  error $ "Parsing error near " ++ n

parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
