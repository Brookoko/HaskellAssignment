module LanguageFunction where

import Language
import Text.ParserCombinators.Parsec

statement' :: Parser Statement
statement' =
  load <|>
  skip

load = do
  reserved "load"
  Load <$> identifier

skip = return Skip

