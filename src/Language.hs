module Language where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Table;

data Statement = Seq [Statement]
  | Load String
  | Select [String] String Statement
  | Skip String
  deriving (Show)

languageDef = emptyDef {
  Token.commentLine = "//",
  Token.identStart = letter <|> oneOf  "_-*",
  Token.identLetter = alphaNum <|> oneOf  "_-*",
  Token.reservedNames = [
    "load",
    "select",
    "from"
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
comma = Token.comma lexer
