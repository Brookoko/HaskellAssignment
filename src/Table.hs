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
  name :: String,
  header :: [String],
  rows :: [[Maybe String]]
}

data Alignment = Left | Right | Center deriving Eq

instance Show Table where
  show (Table name h table) = unlines (intersperse sep (header : rows))
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

fromList name list = Table name (head list) (pack (tail list))
pack = map (map packString)
packString s = if null s then Nothing else Just s

fromTables (Table "" [] []) (Table n h rows) = Table n h rows
fromTables (Table n h rows) (Table "" [] []) = Table n h rows
fromTables (Table n h1 rows1) (Table _ h2 rows2) = Table n (h1 ++ h2) (merge rows1 rows2)
  where
    merge (x:xs) (y:ys) = (x ++ y) : merge xs ys
    merge _ _ = []

tableFromColumn name h t@(Table n header rows)
  | name == "*" = t
  | otherwise = Table n [h] (map (\x -> [x !! i]) rows)
    where i = columnIndex name t

empty = Table "" [] []

isEmpty (Table "" [] []) = True
isEmpty _ = False

columnIndex name (Table n header _) = fromMaybe err (elemIndex name header)
  where err = error ("No column with name: " ++ name)
