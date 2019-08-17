{-
  Exploring equality in Haskell
  Author: Andrew Jarombek
  Date: 8/10/2019
-}

-- The type class 'Eq' defines two functions - (==) and (/=).  (/=) has a default definition in the type class, so
-- instances don't need to override it.  Haskell provides a shortcut for making a type an instance of the Eq type class
-- with the 'deriving' keyword.  deriving also works for Ord, Show and Read.
data Yarn = Yarn String String Int
              deriving Eq

-- My custom WrappingPaper type doesn't use the 'deriving Eq' shortcut, instead explicitly making WrappingPaper
-- an instance of Eq.
data WrappingPaper = WrappingPaper String String

instance Eq WrappingPaper where
  WrappingPaper brand1 pattern1 == WrappingPaper brand2 pattern2 = brand1 == brand2 && pattern1 == pattern2

main :: IO ()
main = do
  -- The following equality checks all print 'True'
  print $ 2 == 2
  print $ 2.2 /= 2.0
  print $ "andy" == "andy"
  print $ Just 4 /= Nothing
  print $ Just [1,2] == Just [1,2]
  print $ ["deer", "chipmunk", "squirrel"] == ["deer", "chipmunk", "squirrel"]

  -- Testing the custom Yarn type which is an instance of the Eq type class.
  let yarn1 = Yarn "Polyester" "Pitter Patter" 70
  let yarn2 = Yarn "Polyester" "Pitter Patter" 70
  let yarn3 = Yarn "Polyester" "Vanilla" 220
  print $ yarn1 == yarn2
  print $ yarn1 /= yarn3