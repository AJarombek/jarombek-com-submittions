#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
  Getting started with Haskell
  Author: Andrew Jarombek
  Date: 8/23/2018
-}

-- Function to subtract one integer from another.  Not that the type definition appears as a higher order function,
-- however 'Int -> Int -> Int' is equivalent to a function with two parameters: 'Int, Int -> Int'
-- Ex. sub 5 3
sub :: Int -> Int -> Int
sub x y = x-y

-- Function to subtract one integer from another from the first two items in a list
-- Ex: subList [5,3]
subList :: [Int] -> Int
subList [x, y] = x-y

-- Function that takes in a tuple type.  The second tuple component is subtracted from the second tuple component.
-- Both components are integers.
-- Ex: subTuple (4,5)
subTuple :: (Int, Int) -> Int
subTuple (x,y) = x-y

-- Display the program result in stdout
-- 'show :: Int -> String' converts a value to a character list (which is aliased as String)
main :: IO ()
main = putStrLn (show (sub 5 3))