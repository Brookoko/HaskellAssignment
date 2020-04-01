module LanguageFunction where

import Language
import Text.ParserCombinators.Parsec

data Statement = Seq [Statement]
  | Load String String
  | Skip
  deriving (Show)

statement' :: Parser Statement
statement' =
  load <|>
  skip

load = do
  reserved "load"
  file <- identifier
  Load file <$> identifier

skip :: Parser Statement
skip = return Skip

