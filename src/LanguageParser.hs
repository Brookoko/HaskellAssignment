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
  reserved "from"
  table <- name
  Select cols table <$> statement

cols = sepBy1 col comma
col = colWithName <|> simpleCol

colWithName = do
  col <- name
  if col == "*" then return $ Col "*" "*"
  else do
    reserved "as"
    Col col <$> name

simpleCol = do
  col <- name
  return $ Col col col

skip = Skip <$> name
end = do
  eof
  return $ Skip "no input"

parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
