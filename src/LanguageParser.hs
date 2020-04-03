module LanguageParser
  (
    parseString
  ) where

import System.IO
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Monad
import Language

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
  skip <|>
  where' <|>
  order <|>
  end

name = stringLiteral <|> identifier

load = do
  reserved "load"
  file <- parens name
  return $ Load file

select = do
  reserved "select"
  distinctSelect <|> simpleSelect False

distinctSelect = do
  try $ reserved "distinct"
  simpleSelect True

simpleSelect dist = do
  cols <- cols
  Select dist cols <$> statement'

cols = sepBy1 col comma

col = do
  col <- name
  colWithName col <|> simpleCol col

colWithName col = do
  try $ reserved "as"
  Col col <$> name

simpleCol col = return $ Col col col

from = do
  reserved "from"
  n <- name
  From n <$> statement'

where' = do
  reserved "where"
  expr <- boolExpression
  Where expr <$> statement'

order = do
  reserved "order"
  reserved "by"
  cols <- orderCols
  OrderBy (reverse cols) <$> statement'

orderCols = sepBy1 orderCol comma

orderCol = do
  col <- name
  ColumnOrder col <$> orderType

orderType =
  (reserved "asc" >> return Ascending) <|>
  (reserved "desc" >> return Descending) <|>
  return Ascending

skip = Skip <$> name
end = do
  eof
  return $ Skip "no input"

parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

arithmeticExpression = buildExpressionParser arithmeticOperators arithmeticTerm
boolExpression = buildExpressionParser boolOperators boolTerm

arithmeticOperators = [
  [Prefix (reservedOp "-" >> return Neg)],
  [Infix (reservedOp "*" >> return (ArithmeticBinary Multiply)) AssocLeft, Infix (reservedOp "/" >> return (ArithmeticBinary Divide)) AssocLeft],
  [Infix (reservedOp "+" >> return (ArithmeticBinary Add)) AssocLeft, Infix (reservedOp "-" >> return (ArithmeticBinary Subtract)) AssocLeft]]

boolOperators = [
  [Prefix (reservedOp "not" >> return Not)],
  [Infix (reservedOp "and" >> return (BoolBinary And)) AssocLeft],
  [Infix (reservedOp "or" >> return (BoolBinary Or)) AssocLeft]]

arithmeticTerm =
  parens arithmeticExpression <|>
  Var <$> name <|>
  IntConst <$> integer

boolTerm = parens boolExpression <|>
  (reserved "true" >> return (BoolConst True)) <|>
  (reserved "false" >> return (BoolConst False)) <|>
  relationExpr

relationExpr = do
  expr <- arithmeticExpression
  relationBinary expr <|> betweenExpression expr

relationBinary expr = do
  op <- relation
  RelationBinary op expr <$> arithmeticExpression

betweenExpression expr1 = do
  reserved "between"
  expr2 <- arithmeticExpression
  reserved "and"
  RelationTernary Between expr1 expr2 <$> arithmeticExpression

relation =
  (reservedOp "<>" >> return NotEqual) <|>
  (reservedOp "=" >> return Equal) <|>
  (reservedOp ">" >> return Greater) <|>
  (reservedOp ">=" >> return GreaterThan) <|>
  (reservedOp "<" >> return Less) <|>
  (reservedOp "<=" >> return LessThan)
