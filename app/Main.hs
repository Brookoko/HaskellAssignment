module Main where

import Console;
import Tablefy;
import Parser;

main :: IO ()
main = prompt showParse

showParse input = do
  table <- parseFile input
  putStrLn $ tablefy' table
