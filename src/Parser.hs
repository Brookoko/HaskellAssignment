module Parser
  (
    Parser,
    parseFile
  ) where

import Table
import Data.List
import Data.List.Split
import System.Directory

data Parser = Parser {
  parse :: String -> Table,
  extension :: String
}

parsers = [ Parser parseCsv "csv",  Parser parseCsv "tsv", Parser parseCsv "json" ]

parseCsv content = Table $ map fromArray (transpose $ splitByComma content)
 where splitByComma content = map (correctQuotes . splitOn ",") (lines content)

correctQuotes list
  | any (isPrefixOf "\"") list = correctQuotes $ concatIntoOne $ break (isPrefixOf "\"") list
  | otherwise = list;
concatIntoOne (first, second) = init first ++ [tail (last first) ++ init (head second)] ++ tail second

parseFile file = do
  isExist <- doesFileExist file
  if isExist
    then do
      content <- readFile file
      return $ parse (parserByFileExtension file) content
    else error $ "No file with name" ++ file

parserByFileExtension file = head $ filter isSuitable parsers
  where isSuitable = canWorkWith (fileExtension file)

canWorkWith fileExtension parser = extension parser == fileExtension;
fileExtension file = last $ splitOn "." file
