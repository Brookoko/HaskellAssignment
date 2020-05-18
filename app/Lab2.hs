module Lab2 where
import Data.List
import Data.String

factorials n = take n $ scanl ((*) . negate) 1 [1..]

quicksort _ [] = []
quicksort comp (x:xs) = (quicksort comp lesser) ++ [x] ++ (quicksort comp greater)
    where
        lesser = filter (`comp` x) xs
        greater = filter (\y -> not (y `comp` x)) xs

sentence = "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
word = words $ filter (`notElem` ",.") sentence;
m (x:xs) = filter (\el -> length el < length x) xs
n (x:xs) = filter (\el -> length el >= length x) xs

main = do {
  print $ factorials 5;
  print $ factorials 13;
  print $ quicksort (<) (factorials 5);
  print $ quicksort (<) (factorials 13);
  print $ quicksort (<) word;
  print $ quicksort (\x y -> length x < length y) word;
}