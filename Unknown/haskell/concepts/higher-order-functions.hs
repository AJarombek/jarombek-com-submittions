{-
  Working with higher-order functions in Haskell
  Author: Andrew Jarombek
  Date: 10/13/2018
-}

{-|
  Redefining the fold-right recursive function for reference
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

main :: IO ()
main = do
  print( odds [1,2,3] ) -- 2
  print( odds [1,3,5,7,11,12] ) -- 5

  print( odds' [1,2,3] ) -- 2
  print( odds' [1,3,5,7,11,12] ) -- 5