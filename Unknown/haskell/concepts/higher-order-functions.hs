{-
  Working with higher-order functions in Haskell
  Author: Andrew Jarombek
  Date: 10/13/2018
-}

{-|
  Redefining the fold-right recursive function for reference
  (a -> b -> b) - a function that takes a list item 'a' and an accumulator 'b' and performs an operation
                    on 'a' that combines it with 'b'
              b - the starting value for the accumulator
            [a] - a list to operate on
-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

{-|
  Redefining the fold-left recursive function for reference
-}
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

{-|
  Count the number of odd numbers in a list using recursion
-}
odds :: Integral a => [a] -> a
odds [] = 0
odds (x:xs) = x `mod` 2 + odds xs

{-|
  Count the number of odd numbers using the foldr function
-}
odds' :: Integral a => [a] -> a
odds' xs = foldr (+) 0 [x `mod` 2 | x <- xs]

{-|
  Count the number of odd numbers in a list using recursion that is associated to the left
-}
odds'' :: Integral a => [a] -> a
odds'' list = calc_odds 0 list
              where
                calc_odds v [] = v
                calc_odds v (x:xs) = calc_odds (v + (x `mod` 2)) xs

{-|
  Count the number of odd numbers using the foldl function
-}
odds''' :: Integral a => [a] -> a
odds''' xs = foldl (+) 0 [x `mod` 2 | x <- xs]

{-|
  Divide the items in a list, basically creating a fraction out of the list
-}
frac :: Integral a => [a] -> a
frac [] = 1
frac (x:xs) = x `div` frac(xs)

{-|
  Divide the items in a list using the foldr function
-}
frac' :: Integral a => [a] -> a
frac' = foldr div 1

{-|
  Divide the items in a list using left associated recursion
-}
frac'' :: Integral a => [a] -> a
frac'' (x:xs) = calc_frac x xs
          where
            calc_frac v [] = v
            calc_frac v (x:xs) = calc_frac (v `div` x) xs

{-|
  Divide the items in a list using the foldl function
-}
frac''' :: Integral a => [a] -> a
frac''' (x:xs) = foldl div x xs

{-|
  Convert a list into an integer number using foldl (and a lambda function)
-}
list2int :: [Int] -> Int
list2int = foldl (\x -> (\y -> x * 10 + y)) 0

{- Main Function -}
main :: IO ()
main = do
  print( odds [1,2,3] ) -- 2
  print( odds [1,3,5,7,11,12] ) -- 5

  print( odds' [1,2,3] ) -- 2
  print( odds' [1,3,5,7,11,12] ) -- 5

  print( odds'' [1,2,3] ) -- 2
  print( odds'' [1,3,5,7,11,12] ) -- 5

  print( odds''' [1,2,3] ) -- 2
  print( odds''' [1,3,5,7,11,12] ) -- 5

  print( frac [12, 3] ) -- 4
  print( frac [4, 2] ) -- 2
  print( frac [20, 4, 2] ) -- 10 - first divide 4 by 2 = 2, then divide 20 by 2 = 10

  print( frac' [12, 3] ) -- 4
  print( frac' [4, 2] ) -- 2
  print( frac' [20, 4, 2] ) -- 10

  print( frac'' [12, 3] ) -- 4
  print( frac'' [4, 2] ) -- 2
  print( frac'' [20, 4, 2] ) -- 2

  print( frac''' [12, 3] ) -- 4
  print( frac''' [4, 2] ) -- 2
  print( frac''' [20, 4, 2] ) -- 2

  print( list2int [2, 2, 6, 1, 9, 9, 5] ) -- 2261995