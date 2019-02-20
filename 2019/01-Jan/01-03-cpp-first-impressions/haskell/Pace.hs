{-
  Create a local module in Haskell
  Author: Andrew Jarombek
  Date: 12/30/2018
-}

module Pace where

  {-|
   - Function to calculate the mile pace of a run
   -}
  calc_pace :: Float -> Int -> Int -> Float
  calc_pace d m s = (fromIntegral ((m * 60) + s)) / d