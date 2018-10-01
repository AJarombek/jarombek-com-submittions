#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
  Showing off some basic concepts of Haskell
  Author: Andrew Jarombek
  Date: 9/7/2018
-}

{-|
  Wrap any value into a list
  A less generic type definition for just values of type Int would be:
  wrap :: Int -> [Int]
-}
wrap :: a -> [a]
wrap a = [a]

{-|
  Join two strings together
-}
join :: String -> String -> String
join a b = a ++ b

{-|
  Determine if a number is higher (older), equal to, or lower (younger) than my age using conditional expressions
-}
older :: Int -> Int
older n = if n > 23 then 1
          else if n == 23 then 0
          else -1

{-|
  Determine if a number is higher (older), equal to, or lower (younger) than my age using guarded equations
-}
older' :: Int -> Int
older' n | n > 23 = 1
         | n == 23 = 0
         | otherwise = -1

{-|
  Function which returns a constant value of my age.  A wildcard pattern is used for the function argument
  since its value does not affect the function return value.
-}
age :: a -> Int
age _ = 23

{-|
  Multiply two numbers together.  Curried functions are easier to understand when a lambda function syntax is used.
-}
mult :: Int -> (Int -> Int)
mult = \x -> (\y -> x * y)

{-|
  The age function which returns a constant value of my age can be rewritten to utilize lambda functions.
-}
age' :: a -> Int
age' = \_ -> 23

{-|
  Get half the length of a list
-}
halveLength :: [a] -> Int
halveLength list = length list `div` 2

{-|
  Split a list into two lists.  Use a tuple as a container for these two lists
-}
halve :: [a] -> ([a], [a])
halve list = splitAt (halveLength list) list

{-|
  Get the third element in a list.  !! is the at-index operator
-}
third :: [a] -> a
third list = list !! 2

{-|
  Get a sub list from a list
-}
subList :: Int -> Int -> [a] -> [a]
subList _ _ [] = []
subList _ 0 _ = []
subList s e (x:xs) | s <= 0 && e > 0 = x : subList s (e - 1) xs
                   | s <= 0 && e <= 0 = []
                   | otherwise = subList (s - 1) (e - 1) xs

{-|
  Sum all the items in a list (squared).  The list can have any items that follow the Num class constraint.
  This function utilizes a list comprehension generator, which follows the mathematical set comprehension notation.
-}
sumListSq :: Num a => [a] -> a
sumListSq xs = sum [x^2 | x <- xs]

{-|
  Extract the vowels from a String.  Uses a guard in the list comprehension which filters the generator
-}
vowels :: String -> String
vowels str = [s | s <- str, elem s "aeiouAEIOU"]

{-|
  Create a list of grid indices for a grid of a certain length and height
-}
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a, b) | a <- [0..x], b <- [0..y]]

{-|
  Get the grid coordinates for the perimeter of a square.
-}
square :: Int -> [(Int, Int)]
square x = [(a, b) | (a, b) <- grid x x, a == 0 || b == 0 || a == x || b == x]

{-|
  Use recursion to double the contents of a list.
-}
dubList :: Num a => [a] -> [a]
dubList [] = []
dubList (x:xs) = x * 2 : dubList xs

main :: IO()
main = do
  putStrLn (show (wrap "Hello")) -- ["Hello"]
  putStrLn (show (wrap 2)) -- [2]

  putStrLn (show (join "Hey" "There")) -- "HeyThere"
  -- Strings in haskell are simply an alias for character arrays
  putStrLn (show (join ['h', 'e', 'y'] ['t', 'h', 'e', 'r', 'e'])) -- "heythere"

  putStrLn (show (older 24)) -- 1
  putStrLn (show (older 23)) -- 0
  putStrLn (show (older 22)) -- -1

  putStrLn (show (older' 24)) -- 1
  putStrLn (show (older' 23)) -- 0
  putStrLn (show (older' 22)) -- -1

  putStrLn (show (age "?")) -- 23
  putStrLn (show (age 40)) -- 23

  putStrLn (show (mult 2 3)) -- 6

  putStrLn (show (age' 40)) -- 23

  putStrLn (show (halve [1,2,3,4,5,6])) -- ([1,2,3],[4,5,6])

  putStrLn (show (third [1,2,3,4])) -- 3

  putStrLn (show (subList 2 4 [0,1,2,3,4,5,6,7])) -- [2,3]
  putStrLn (show (subList 0 1 [0,1,2,3,4,5,6,7])) -- [0]
  putStrLn (show (subList 5 11 [0,1,2,3,4,5,6,7])) -- [5,6,7]
  putStrLn (show (subList 0 (-1) [0, 2, 3, 5])) -- []
  putStrLn (show (subList 0 4 "Andy Jarombek")) -- "Andy"
  putStrLn (show (subList 0 6 "Andy Jarombek")) -- "Andy J"
  putStrLn (show (subList 5 14 "Andy Jarombek")) -- "Jarombek"

  putStrLn (show (sumListSq [1,2,3,4])) -- 30
  putStrLn (show (sumListSq [1.2, 2.5, 3, 4.9])) -- 40.7...

  putStrLn (show (vowels "Andy Jarombek")) -- "Aaoe"
  putStrLn (show (vowels "Greenwich,CT")) -- "eei"

  putStrLn (show (grid 2 3)) -- [(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3)]
  putStrLn (show (grid 0 0)) -- [(0,0)]

  putStrLn (show (square 1)) -- [(0,0),(0,1),(1,0),(1,1)]
  putStrLn (show (square 2)) -- [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]
  putStrLn (show (square 3)) -- [(0,0),(0,1),(0,2),(0,3),(1,0),(1,3),(2,0),(2,3),(3,0),(3,1),(3,2),(3,3)]

  putStrLn (show (dubList [2,4,6])) -- [4,8,12]