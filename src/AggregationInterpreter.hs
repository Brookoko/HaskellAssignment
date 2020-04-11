module AggregationInterpreter
  (
    evaluate
  ) where

import Language
import Table
import Comparable
import Data.List hiding (sum)
import Prelude hiding (min, max, sum)
import TableFunctions

evaluate f@(AggregationColumn _ (ColumnDistinct isDistinct name) h) t@(Table n header rows) =
  Table n [h] [[aggregate f (map head (tryDistinctRows isDistinct rows))]]

aggregate (AggregationColumn Count _ _) list = Just $ show $ length list
aggregate (AggregationColumn Min _ _) list = min list
aggregate (AggregationColumn Max _ _) list = max list
aggregate (AggregationColumn Avg _ _) list = Just $ show $ average list
aggregate (AggregationColumn Sum _ _) list = Just $ show $ sum list

min = minimumBy comp
max = maximumBy comp
comp x y = compare (convert x) (convert y)
convertList = map convert
sum = foldr ((+) . toInt) 0 . convertList
average list = realToFrac(sum list) / realToFrac(length list)

