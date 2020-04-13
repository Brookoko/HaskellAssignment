module Console where

import Control.Monad (unless)
import Parser

prompt onInput = do
  line <- getLine
  let fixed = unwords $ words line
  print fixed
  unless (line == ":q") $ do
    onInput fixed
    prompt onInput
