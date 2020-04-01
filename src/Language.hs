module Language where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = emptyDef {
  Token.commentLine = "//",
  Token.identStart = letter <|> char '/' <|> char '-' <|> char '_' <|> char '.',
  Token.identLetter = alphaNum <|> char '/' <|> char '-' <|> char '_' <|> char '.',
  Token.reservedNames = [
    "load"
  ]
}

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
