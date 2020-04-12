module Main where

import Console
import LanguageParser
import Interpreter

main :: IO ()
main = prompt showParse

showParse input = do
  let statement = parseString input
  print statement
  res <- execute statement
  putStrLn res
