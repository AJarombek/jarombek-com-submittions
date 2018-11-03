{-
  Work with Haskell compositions
  Author: Andrew Jarombek
  Date: 11/2/2018
-}

import Data.List

{-|
  Function that performs a Map-Reduce operation on an integer list.
  Each item is first incremented and then the sum of this list is returned.
-}
inc_sum :: [Int] -> Int
inc_sum = sum . map (+1)

{-|
  Take a list where each item is of class Show.  First convert each item into a string
  and add a space between each item.  Then concat the list of strings, making one string.
  Finally print out the string.
-}
list_str :: Show a => [a] -> IO ()
list_str = print . concat . intersperse " " . map show

{-|
  Alternatively apply two functions to successive elements in a list.
-}
alt_map :: Integral a => (a -> b) -> (a -> b) -> [a] -> [b]
alt_map f g = tuple_map f g . partition even

{-|
  Map one function to the first tuple element (a list) and map another function to
  the second tuple element (a list).  Then interleave both lists into a single list.
-}
tuple_map :: (a -> b) -> (a -> b) -> ([a],[a]) -> [b]
tuple_map f g (x, y) = interleave (map f x) (map g y)

{-|
  Interleave two lists together, producing a single list.  Lists do not need to be of equal length.
-}
interleave :: [a] -> [a] -> [a]
interleave [] y = y
interleave x [] = x
interleave (x:xs) [y] = x : y : xs
interleave [x] (y:ys) = x : y : ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

{- Main Function -}
main :: IO ()
main = do
  -- The $ operator allows parenthesis to be removed
  print $ "..Inside Main Function.."
  print $ inc_sum [0,1] -- 3

  list_str [True, False] -- "True False"
  list_str [2, 26, 1995] -- "2 26 1995"

  print $ alt_map (+10) (+100) [1,2,3,4,5] -- [12,101,14,103,105]

  print $ tuple_map (*2) (+1) ([2], [2]) -- [4,3]
  print $ tuple_map (*2) (+1) ([2,2,2], [2,2,2]) -- [4,3,4,3,4,3]

  print $ interleave [1,2,3] [4,5,6] -- [1,4,2,5,3,6]
  print $ interleave [1,2,3] [4,5,6,7] -- [1,4,2,5,3,6,7]
  print $ interleave [1,2,3] [4,5,6,7,8] -- [1,4,2,5,3,6,7,8]
  print $ interleave [1,2,3,4] [5,6,7] -- [1,5,2,6,3,7,4]
  print $ interleave [1,2,3,4,5] [6,7,8] -- [1,6,2,7,3,8,4,5]
  print $ interleave [1,2,3] [] -- [1,2,3]
  print $ interleave [] [4,5,6] -- [4,5,6]
  print $ interleave [1] [2] -- [1,2]