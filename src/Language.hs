module Language where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Table;

data BoolExpr = BoolConst Bool
  | Not BoolExpr
  | BoolBinary BoolBinaryOp BoolExpr BoolExpr
  | RelationBinary RelationBinaryOp ArithmeticExpr ArithmeticExpr
  | RelationTernary RelationTernaryOp ArithmeticExpr ArithmeticExpr ArithmeticExpr
  deriving (Show)

data BoolBinaryOp = And | Or deriving (Show)

data RelationBinaryOp = Equal | NotEqual | Greater | GreaterThan | Less | LessThan deriving (Show)
data RelationTernaryOp = Between deriving (Show)

data ArithmeticExpr = Var String
  | IntConst Integer
  | Neg ArithmeticExpr
  | ArithmeticBinary ArithmeticBinaryOp ArithmeticExpr ArithmeticExpr
  deriving (Show)

data ArithmeticBinaryOp = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data AggregationFunction = Min | Max | Avg | Sum | Count deriving (Show)

data Statement = Seq [Statement]
  | Load String
  | Select Bool [Language.Column] Statement
  | From String String Statement
  | Where BoolExpr Statement
  | OrderBy [Language.Column] Statement
  | End
  deriving (Show)

data Column = ColumnSimple String
  | ColumnWithName String String
  | ColumnOrder String OrderType
  | ColumnDistinct Bool String
  | AggregationColumn AggregationFunction Language.Column String
  deriving (Show)

data ColumnName = ColumnAndTable String String | JustColumn String deriving (Show)

data OrderType = Ascending | Descending deriving (Show)

languageDef = emptyDef {
  Token.commentLine = "--",
  Token.identStart = letter <|> oneOf  "_-*",
  Token.identLetter = alphaNum <|> oneOf  "_-*",
  Token.reservedNames = [
    "load",
    "select",
    "from",
    "as",
    "distinct",
    "where",
    "null",
    "not",
    "and",
    "or",
    "between",
    "true",
    "false",
    "order",
    "by",
    "asc",
    "desc",
    "count",
    "min",
    "max",
    "avg",
    "med",
    "sum"
  ],
  Token.reservedOpNames = [
    "+", "-", "*", "/", "=",
    "<", "<=", ">", ">=", "<>",
    "and", "or", "not", "between"
  ],
  Token.caseSensitive = False
}

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
stringLiteral = Token.stringLiteral lexer
integer = Token.integer lexer
comma = Token.comma lexer

name = stringLiteral <|> identifier
