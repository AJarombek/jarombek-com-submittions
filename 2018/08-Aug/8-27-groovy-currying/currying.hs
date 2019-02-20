#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
  Demonstrate basic currying in Haskell
  Author: Andrew Jarombek
  Date: 8/24/2018
-}

-- Curried function that multiples two numbers
-- The type of 'mult' can also be written as 'Int -> Int -> Int'
mult :: Int -> (Int -> Int)
mult x y = x * y

-- A partial implementation of the 'mult' function that doubles a number
dub :: Int -> Int
dub = mult 2

-- Another partial implementation that multiplies a number by 10
x10 :: Int -> Int
x10 = mult 10

main :: IO ()
main = putStrLn (show (x10 3)) -- 30