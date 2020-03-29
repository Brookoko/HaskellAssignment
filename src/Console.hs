module Console where

import Control.Monad (unless)
import Parser

prompt onInput = do
  line <- getLine
  unless (line == ":q") $ do
      onInput line
      prompt onInput
