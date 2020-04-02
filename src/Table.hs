module Table
  (
    Table ( .. ),
    empty,
    fromList,
    isEmpty,
    select,
    distinct
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

fromTables (Table h1 rows1) (Table h2 rows2) = Table (h1 ++ h2) (merge rows1 rows2)
merge (x:xs) (y:ys) = (x ++ y) : merge xs ys
merge [] ys = ys

empty = Table [] []

isEmpty (Table [] []) = True
isEmpty _ = False

select cols names table = sel cols names empty
  where
    sel [] _ t = t
    sel (x:xs) (y:ys) t
      | x == "*" = sel xs ys (fromTables t table)
      | otherwise = sel xs ys (fromTables t column)
      where
        column = tableFromColumn x y table

tableFromColumn name h (Table header rows) = Table [h] (map (\x -> [x !! i]) rows)
  where
    i = fromMaybe err (elemIndex name header)
    err = error ("No column with name: " ++ name)

distinct (Table header rows) = Table header (dist rows)
  where
    dist (x:xs) = x : dist (filter (/=x) xs)
    dist _ = []
