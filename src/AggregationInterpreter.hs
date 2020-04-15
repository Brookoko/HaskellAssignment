module AggregationInterpreter
  (
    evaluate,
    aggregate
  ) where

import Language
import Table
import Comparable
import Data.List hiding (sum)
import Prelude hiding (min, max, sum)
import TableFunctions

evaluate (AggregationColumn f (ColumnDistinct isDistinct name) h) t@(Table n header rows groups) i =
  if null groups
  then Table n [h] (aggregateString f isDistinct rows) groups
  else Table n [h] (aggregateMap f isDistinct i groups) groups

aggregateMap :: AggregationFunction -> Bool -> Int -> [[[Maybe String]]] -> [[Maybe String]]
aggregateMap f isDistinct i = map ((:[]) . toString . aggregate f . flatColumn isDistinct i)
aggregateString f isDistinct rows = [[toString $ aggregate f (flatColumn isDistinct 0 rows)]]

flatColumn isDistinct i rows = map (!! i) (tryDistinctRows isDistinct rows)
toString x = Just $ show x

aggregate :: AggregationFunction -> [Maybe String] -> Double
aggregate Count = count
aggregate Min = min
aggregate Max = max
aggregate Avg = average
aggregate Sum = sum

count = realToFrac . length
min = strToDouble . minimumBy comp
max = strToDouble . maximumBy comp
comp x y = compare (convert x) (convert y)
strToDouble = realToFrac . toInt . convert
convertList = map convert
sum = realToFrac . foldr ((+) . toInt) 0 . convertList
average list = sum list / count list

