module Table
  (
    Table ( .. ),
    empty,
    fromList,
    fromTables,
    tableFromColumn,
    isEmpty,
    columnIndex
  ) where

import Data.List;
import Prelude hiding (Left, Right)
import Data.Maybe

data Table = Table {
  header :: [String],
  rows :: [[Maybe String]]
}

data Alignment = Left | Right | Center deriving Eq

instance Show Table where
  show (Table h table) = unlines (intersperse sep (header : rows))
    where
      t = map (map unpack) table
      widths = map (min 60 . maximum . map length) (transpose (h:t))
      sep = intercalate "+" $ map (flip replicate '-' . (+2)) widths
      header = mkRow Center h
      rows = map (mkRow Left) t
      mkRow a = intercalate "|" . zipWith (mkCell a) widths
      mkCell a n xs = " " ++ pad a n ' ' xs ++ " "
      unpack = fromMaybe "null"

pad a n x xs
  | n < 1          = error "pad: Length must not be smaller than one"
  | n <= length xs = take n xs
  | a == Left      = xs ++ fill
  | a == Right     = fill ++ xs
  | a == Center    = let (front, back) = splitAt (diff `div` 2) fill
                     in front ++ xs ++ back
  where
    fill = replicate diff x
    diff = n - length xs

fromList list = Table (head list) (pack (tail list))
pack = map (map packString)
packString s = if null s then Nothing else Just s

fromTables (Table [] []) (Table h rows) = Table h rows
fromTables (Table h rows) (Table [] []) = Table h rows
fromTables (Table h1 rows1) (Table h2 rows2) = Table (h1 ++ h2) (merge rows1 rows2)
  where
    merge (x:xs) (y:ys) = (x ++ y) : merge xs ys
    merge _ _ = []

tableFromColumn name h t@(Table header rows)
  | name == "*" = t
  | otherwise = Table [h] (map (\x -> [x !! i]) rows)
    where i = columnIndex name t

empty = Table [] []

isEmpty (Table [] []) = True
isEmpty _ = False

columnIndex name (Table header _) = fromMaybe err (elemIndex name header)
  where err = error ("No column with name: " ++ name)
