{-
  Compare the Haskell fold operations
  Author: Andrew Jarombek
  Date: 10/15/2018
-}

import Data.List

{-|
  Visualize the execution of the foldr function.
  Inspiration from: https://wiki.haskell.org/Fold#Examples
-}
debug_foldr :: [String] -> String -> String
debug_foldr xs s = foldr (\x y -> "(" ++ x ++ "+" ++ y ++ ")") s xs

{-|
  Visualize the execution of the foldl function
-}
debug_foldl :: [String] -> String -> String
debug_foldl xs s = foldl (\x y -> "(" ++ x ++ "+" ++ y ++ ")") s xs

{-|
  Visualize the execution of the foldl' function
-}
debug_foldl' :: [String] -> String -> String
debug_foldl' xs s = foldl' (\x y -> "(" ++ x ++ "+" ++ y ++ ")") s xs

{- Main Function -}
main :: IO ()
main = do
  print( debug_foldr (map show [1..10]) "0" )  -- "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"
  print( debug_foldl (map show [1..10]) "0" )  -- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
  print( debug_foldl' (map show [1..10]) "0" ) -- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"

  -- Both foldr and foldl give stack overflows for folding over large lists.
  -- This is due to Haskell's lazy reduction strategy.
  -- print( foldr (+) 0 [0..100000000] ) -- *** Exception: stack overflow
  -- print( foldl (+) 0 [0..100000000] ) -- *** Exception

  -- foldl' from the Data.List module uses an immediate reduction strategy.
  -- Because of the immediate reduction, foldl' can operate on large lists without overflowing the stack
  print( foldl' (+) 0 [0..100000000] ) -- 5000000050000000