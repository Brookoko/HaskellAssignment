module Comparable where

import Data.Maybe
import Text.Read

data Comparable = CompInt Integer | CompString (Maybe String)

instance Eq Comparable where
  (CompInt x) == (CompInt y) = x == y
  (CompString x) == (CompString y) = x == y

instance Ord Comparable where
  compare (CompInt x) (CompInt y) = compare x y
  compare (CompString x) (CompString y) = compare x y
  compare (CompInt x) (CompString y) = LT
  compare (CompString x) (CompInt y) = GT

convert x = maybe (CompString x) CompInt (readMaybe (fromMaybe "" x))

toInt (CompInt x) = x
toInt _ = 0
