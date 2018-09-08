#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
  Showing off some basic concepts of Haskell
  Author: Andrew Jarombek
  Date: 9/7/2018
-}

{-
 - Wrap any value into a list
 - A less generic type definition for just values of type Int would be:
 - wrap :: Int -> [Int]
 -}
wrap :: a -> [a]
wrap a = [a]

{-
 - Join two strings together
 -}
join :: String -> String -> String
join a b = a ++ b

{-
 - Determine if a number is higher (older), equal to, or lower (younger) than my age using conditional expressions
 -}
older :: Int -> Int
older n = if n > 23 then 1
          else if n == 23 then 0
          else -1

{-
 - Determine if a number is higher (older), equal to, or lower (younger) than my age using guarded equations
 -}
older' :: Int -> Int
older' n | n > 23 = 1
         | n == 23 = 0
         | otherwise = -1

{-
 - Function which returns a constant value of my age.  A wildcard pattern is used for the function argument
 - since its value does not affect the function return value.
 -}
age :: a -> Int
age _ = 23

{-
 - Multiply two numbers together.  Curried functions are easier to understand when a lambda function syntax is used.
 -}
mult :: Int -> (Int -> Int)
mult = \x -> (\y -> x * y)

{-
 - The age function which returns a constant value of my age can be rewritten to utilize lambda functions.
 -}
age' :: a -> Int
age' = \_ -> 23

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