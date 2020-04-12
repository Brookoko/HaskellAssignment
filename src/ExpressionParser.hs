module ExpressionParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Monad
import Language
import ColumnParser

arithmeticExpression = buildExpressionParser arithmeticOperators arithmeticTerm
boolExpression = buildExpressionParser boolOperators boolTerm
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

boolTerm = parens boolExpression <|>
  (reserved "true" >> return (BoolConst True)) <|>
  (reserved "false" >> return (BoolConst False)) <|>
  try relationString <|>
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
