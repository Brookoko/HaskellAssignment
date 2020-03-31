module Main where

import Console;
import Parser;

main :: IO ()
main = prompt showParse

showParse input = do
  table <- parseFile input
  print table
