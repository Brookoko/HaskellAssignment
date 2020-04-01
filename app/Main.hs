module Main where

import Console;
import LanguageParser;
import Interpreter;

main :: IO ()
main = prompt showParse

showParse input = do
  let statement = parseString input
  table <- execute statement
  print table
