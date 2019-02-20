{-
  Explore compile time functions in Haskell
  Author: Andrew Jarombek
  Date: 12/30/2018
-}

-- Template Haskell is an extension of Haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax (lift)
import Pace

{-|
 - Compile time function to calculate the mile pace of a run
 -}
calc_pace_compile_time :: Float
calc_pace_compile_time = $(lift (calc_pace 2.0 12 31))

{- Main Function -}
main :: IO ()
main = do
  print $ calc_pace_compile_time