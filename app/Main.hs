module Main where

import Lib;
import Tablefy;

main :: IO ()
main = prompt showParse

showParse input = do
  table <- parseFile input
  putStrLn $ tablefy' table