module LanguageParser
  (
    parseString
  ) where

import System.IO
import Text.ParserCombinators.Parsec
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
  end

name = stringLiteral <|> identifier

load = do
  reserved "load"
  file <- parens name
  return $ Load file

select = do
  reserved "select"
  cols <- cols
  Select cols <$> statement'

cols = sepBy1 col comma
col = do
  col <- name
  colWithName col <|> simpleCol col

colWithName col = do
  try $ reserved "as"
  n <- name
  return $ Col col n False

simpleCol col = return $ Col col col False

from = do
  reserved "from"
  n <- name
  From n <$> statement'

skip = Skip <$> name
end = do
  eof
  return $ Skip "no input"

parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
