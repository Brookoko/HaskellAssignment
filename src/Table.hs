module Table
  (
    Table,
    fromList
  ) where

import Data.List;
import Prelude hiding (Left, Right)

data Table = Table [String] [[Maybe String]]

instance Show Table where
  show (Table h table) = unlines $ intersperse sep (header:rows)
    where
      t = map (map unpack) table
      widths = map (maximum . map length) (transpose (h:t))
      sep = intercalate "+" $ map (flip replicate '-' . (+2)) widths
      header = mkRow Center h
      rows = map (mkRow Left) t
      mkRow a = intercalate "|" . zipWith (mkCell a) widths
      mkCell a n xs = " " ++ pad a n ' ' xs ++ " "
      unpack (Just x) = x
      unpack _  = "null"

fromList list = uncurry Table (head list, pack $ tail list)
pack = map (map packString)
packString s = if null s then Nothing else Just s

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

