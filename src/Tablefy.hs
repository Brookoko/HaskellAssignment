module Tablefy
  (
    tablefy,
    tablefy'
  ) where

import Prelude hiding (Left, Right)
import Data.List (intercalate, intersperse, transpose)
import Table;

tablefy' (Table columns) = tablefy (fst col) (transpose $ unpack $ snd col)
  where
    col = unzip $ map (\x -> (header x, content x)) columns
    unpack = map (map unpack')
    unpack' (Just x) = x
    unpack' _ = "null"

tablefy h rs = unlines $ intersperse sep (header:rows)
  where
    widths = map (maximum . map length) (transpose (h:rs))
    sep = intercalate "+" $ map (flip replicate '-' . (+2)) widths
    header = mkRow Center h
    rows = map (mkRow Left) rs
    mkRow a = intercalate "|" . zipWith (mkCell a) widths
    mkCell a n xs = " " ++ pad a n ' ' xs ++ " "

data Alignment = Left | Right | Center deriving Eq

pad a n x xs
    | n < 1          = error "Tablefy.pad: Length must not be smaller than one"
    | n <= length xs = take n xs
    | a == Left      = xs ++ fill
    | a == Right     = fill ++ xs
    | a == Center    = let (front, back) = splitAt (diff `div` 2) fill
                       in front ++ xs ++ back
    where
        fill = replicate diff x
        diff = n - length xs
