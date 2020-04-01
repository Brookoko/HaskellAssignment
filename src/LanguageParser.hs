module LanguageParser where

import System.IO
import Text.ParserCombinators.Parsec
import Language
import LanguageFunction

whileParser = whiteSpace >> statement

statement = parens statement <|> seqStatement
seqStatement = do
  list <- sepBy1 statement' semi
  return $ if length list == 1 then head list else Seq list

parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
