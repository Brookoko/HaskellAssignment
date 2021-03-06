module Language where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Table;

data BoolExpr = BoolConst Bool
  | Not BoolExpr
  | BoolBinary BoolBinaryOp BoolExpr BoolExpr
  | RelationBinary RelationBinaryOp ArithmeticExpr ArithmeticExpr
  | RelationBinaryString RelationBinaryOp StringExpr StringExpr
  | RelationTernary RelationTernaryOp ArithmeticExpr ArithmeticExpr ArithmeticExpr
  | RelationAggregation RelationBinaryOp AggregationExpr AggregationExpr
  deriving (Show)

data BoolBinaryOp = And | Or deriving (Show)

data RelationBinaryOp = Equal | NotEqual | Greater | GreaterThan | Less | LessThan deriving (Show)
data RelationTernaryOp = Between deriving (Show)

data ArithmeticExpr = Var ColumnName
  | IntConst Integer
  | Neg ArithmeticExpr
  | ArithmeticBinary ArithmeticBinaryOp ArithmeticExpr ArithmeticExpr
  deriving (Show)

data ArithmeticBinaryOp = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data StringExpr = VarString ColumnName
  | StringConst String
  deriving (Show)

data AggregationExpr = VarInt Integer
  | AggregationExpression AggregationFunction ColumnName
  deriving (Show)

data Statement = Seq [Statement]
  | Load String
  | Select Bool [Language.Column] Statement
  | From TableName Statement
  | Where BoolExpr Statement
  | OrderBy [Language.Column] Statement
  | InnerJoin TableName BoolExpr Statement
  | FullJoin TableName BoolExpr Statement
  | LeftJoin TableName BoolExpr Statement
  | RightJoin TableName BoolExpr Statement
  | Group [ColumnName] Statement
  | Having BoolExpr Statement
  | End
  deriving (Show)

data TableName = TableName String String deriving(Show)

data Column = ColumnSimple ColumnName
  | ColumnWithName ColumnName String
  | ColumnOrder ColumnName OrderType
  | ColumnDistinct Bool ColumnName
  | AggregationColumn AggregationFunction Language.Column String
  | CaseColumn [CaseBranch] (Maybe String) String
  deriving (Show)

data ColumnName = ColumnAndTable String String | JustColumn String deriving(Show)

data AggregationFunction = Min | Max | Avg | Sum | Count deriving (Show)

data OrderType = Ascending | Descending deriving (Show)

data CaseBranch = Branch BoolExpr String deriving (Show)

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
    "sum",
    "inner",
    "join",
    "on",
    "full",
    "outer",
    "left",
    "right",
    "case",
    "when",
    "then",
    "else",
    "end",
    "group",
    "having"
  ],
  Token.reservedOpNames = [
    "+", "-", "*", "/", "=",
    "<", "<=", ">", ">=", "<>"
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
dot = Token.dot lexer

string = between (char '\'') (char '\'') identifier

name = stringLiteral <|> identifier

columnName = do
  n <- Language.name
  d <- hasDot
  if d then tableAndColumn n else justColumn n

hasDot = (dot >> return True) <|> return False
tableAndColumn table = ColumnAndTable table <$> Language.name
justColumn name = return $ JustColumn name
