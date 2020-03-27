module Lib where

import Control.Monad (unless)
import System.Directory
import Data.List.Split;
import Data.List;

data Parser = Parser {
  parse :: String -> Table,
  extension :: String
}

newtype Table = Table [Column] deriving (Show)
data Column = Column {
  header :: String,
  content :: [Maybe String]
} deriving (Show)

parsers = [ Parser parseCsv "csv",  Parser parseCsv "tsv", Parser parseCsv "json" ]
fromArray = unpack . uncons
 where
  unpack (Just (name, d)) = Column name (map pack d)
  pack x = if null x then Nothing else Just x    

parseCsv :: String -> Table
parseCsv content = Table $ map fromArray (transpose $ splitByComma content)
 where splitByComma content = map (correctQuotes . splitOn ",") (lines content)

correctQuotes list
  | any (isPrefixOf "\"") list = correctQuotes $ concatIntoOne $ break (isPrefixOf "\"") list
  | otherwise = list;
concatIntoOne (first, second) = init first ++ [tail (last first) ++ init (head second)] ++ tail second

prompt onInput = do
  line <- getLine
  unless (line == ":q") $ do
      onInput line
      prompt onInput

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
