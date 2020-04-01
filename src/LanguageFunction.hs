module LanguageFunction where

import Language
import Text.ParserCombinators.Parsec

statement' :: Parser Statement
statement' =
  load <|>
  skip

load = do
  reserved "load"
  file <- parens name
  return $ Load file

name = stringLiteral <|> identifier

skip = Skip <$> name

