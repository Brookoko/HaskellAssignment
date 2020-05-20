module Lab1 where

import Text.Show.Functions
import Data.List
import Data.Eq

data Type = StringItem String
  | NumItem Int
  | ListItem [Type]
  deriving (Show, Eq)

firstList = [ StringItem "G55", StringItem "G66", StringItem "G77" ]

secondList = [ NumItem 9, ListItem [StringItem "F", StringItem "G" ], StringItem "I"]

thirdList = [ StringItem "N", StringItem "I", StringItem "L", StringItem "T", StringItem "D", StringItem "J",
              ListItem [StringItem "II", StringItem "JJ"] ]

newListOutOfThreeByPosition firstList firstPosition secondList secondPosition thirdList thirdPosition = [
  firstList !! (firstPosition - 1),
  secondList !! (secondPosition - 1),
  thirdList !! (thirdPosition - 1) ]

set a b c = not (intersect (a \\ (not b)) (union (not a) b))
  where not = (universe \\)
        universe = union (union a b) c

main :: IO ()
main = do
  putStrLn "First task:"
  print ((\first second third -> [ head first, head second, head third ]) firstList secondList thirdList)
  putStrLn "Second task:"
  print (newListOutOfThreeByPosition firstList 3 secondList 2 thirdList 6)
  putStrLn "Third task:"
  print (set firstList secondList thirdList)

  putStrLn "union example:"
  print (union ["hello", "world"] ["xxi", "century", "world"])
  putStrLn "intersect example:"
  print (intersect ["hello", "world"] ["xxi", "century", "world"])
  putStrLn "set difference example:"
  print (["hello", "world"] \\ ["xxi", "century", "world"])