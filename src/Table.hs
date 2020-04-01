module Table
  (
    Table ( .. ),
    empty,
    fromList,
    select
  ) where

import Data.List;
import Prelude hiding (Left, Right)
import Data.Maybe

data Table = Table {
  header :: [String],
  rows :: [[Maybe String]]
}

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

fromList list = Table (head list) (pack (tail list))
pack = map (map packString)
packString s = if null s then Nothing else Just s

empty = Table [] []

select cols table = sel cols empty
  where
    sel [] t = t
    sel (x:xs) t@(Table h rows)
      | x == "*" = sel xs (sel (header table) t)
      | otherwise = sel xs (Table (h ++ [h']) (merge rows rows'))
      where (h', rows') = column x table

column name (Table header rows) = (header !! i, map (\x -> [x !! i]) rows)
  where
    i = fromMaybe err  (elemIndex name header)
    err = error ("No column with name: " ++ name)

merge (x:xs) (y:ys) = (x ++ y) : merge xs ys
merge [] ys = ys

data Alignment = Left | Right | Center deriving Eq

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

