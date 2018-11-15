{-
  Work with the Haskell newtype keyword
  Author: Andrew Jarombek
  Date: 11/14/2018
-}

-- A type (created with the 'data' keyword) that has a single constructor and a single argument
-- can be rewritten as a 'newtype'.
newtype Miles = Miles Float
                deriving (Show, Read)

newtype Kilometers = Kilometers Float
                     deriving (Show, Read)

newtype Meters = Meters Float
                 deriving (Show, Read)

{- Main Function -}
main :: IO ()
main = do
  print $ Miles 3.11
  print $ Kilometers 5
  print $ Meters 5000
