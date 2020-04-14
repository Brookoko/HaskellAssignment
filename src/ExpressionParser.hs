module ExpressionParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Monad
import Language

arithmeticExpression = buildExpressionParser arithmeticOperators arithmeticTerm
boolExpression = buildExpressionParser boolOperators boolTerm
boolExpressionHaving = buildExpressionParser boolOperators boolTermHaving
stringExpression = VarString <$> columnName <|> StringConst <$> Language.string

arithmeticOperators = [
  [Prefix (reservedOp "-" >> return Neg)],
  [Infix (reservedOp "*" >> return (ArithmeticBinary Multiply)) AssocLeft, Infix (reservedOp "/" >> return (ArithmeticBinary Divide)) AssocLeft],
  [Infix (reservedOp "+" >> return (ArithmeticBinary Add)) AssocLeft, Infix (reservedOp "-" >> return (ArithmeticBinary Subtract)) AssocLeft]]

boolOperators = [
  [Prefix (reserved "not" >> return Not)],
  [Infix (reserved "and" >> return (BoolBinary And)) AssocLeft],
  [Infix (reserved "or" >> return (BoolBinary Or)) AssocLeft]]

arithmeticTerm =
  parens arithmeticExpression <|>
  Var <$> columnName <|>
  IntConst <$> integer

boolTerm = parens boolExpression <|> boolTerms

boolTerms =
  (reserved "true" >> return (BoolConst True)) <|>
  (reserved "false" >> return (BoolConst False)) <|>
  try relationString <|>
  try relationExpr

boolTermHaving = parens boolExpressionHaving <|>
  boolTerms <|>
  aggregationExpr

aggregationExpr = do
  expr <- aggregate
  op <- relation
  RelationAggregation op expr <$> aggregate

aggregate = aggregationFuncExpr <|> aggregationInt

aggregationFuncExpr = do
  function <- aggregationFunction
  col <- parens columnName
  return $ AggregationExpression function col

aggregationInt = VarInt <$> integer

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

relationString = do
  expr <- stringExpression
  op <- relation
  RelationBinaryString op expr <$> stringExpression

relation =
  (reservedOp "<>" >> return NotEqual) <|>
  (reservedOp "=" >> return Equal) <|>
  (reservedOp ">" >> return Greater) <|>
  (reservedOp ">=" >> return GreaterThan) <|>
  (reservedOp "<" >> return Less) <|>
  (reservedOp "<=" >> return LessThan)

aggregationFunction =
  (reserved "count" >> return Count) <|>
  (reserved "min" >> return Min) <|>
  (reserved "max" >> return Max) <|>
  (reserved "avg" >> return Avg) <|>
  (reserved "sum" >> return Sum)
