module Parser
  (
    Parser,
    parseFile
  ) where

import Table
import Data.List
import Data.List.Split
import System.Directory
import Data.Maybe

data Parser = Parser {
  parse :: String -> Table,
  extension :: String
}

parsers = [ Parser parseCsv "csv",  Parser parseTsv "tsv", Parser parseCsv "json" ]

parseTsv = fromList . splitByTab
  where splitByTab content = map (splitOn "\t") (lines content)

parseCsv content = fromList $ splitByComma content
  where splitByComma content = map (correctQuotes . splitOn ",") (lines content)

correctQuotes = map correct . joinStringWithCommas
  where
    correct s
      | ("\"" `isPrefixOf` s) && ("\"" `isSuffixOf` s) = correct $ tail $ init s
      | "\"\"" `isInfixOf` s = replace "\"\"" "\"" s
      | otherwise = s

joinStringWithCommas (x:xs)
  | ("\"" `isPrefixOf` x) && not ("\"" `isSuffixOf` x) = correct x xs : joinStringWithCommas (drop (index x xs + 1) xs)
  | otherwise = x : joinStringWithCommas xs
  where
    correct x xs = intercalate "," (x : take (index x xs + 1) xs)
    index x xs = fromMaybe (-1) (findIndex ("\"" `isSuffixOf`) xs)
joinStringWithCommas _ = []

replace x y s
  | x `isPrefixOf` s = y ++ replace x y (drop (length x) s)
  | null s = ""
  | otherwise = head s : replace x y (tail s)

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
