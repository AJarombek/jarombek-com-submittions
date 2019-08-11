{-
  Exploring equality in Haskell
  Author: Andrew Jarombek
  Date: 8/10/2019
-}

data Yarn = Yarn String String Int
              deriving Eq

main :: IO ()
main = do
  -- The following equality checks all print 'True'
  print $ 2 == 2
  print $ 2.2 /= 2.0
  print $ "andy" == "andy"
  print $ Just 4 /= Nothing
  print $ Just [1,2] == Just [1,2]

  let yarn1 = Yarn "Polyester" "Pitter Patter" 70
  let yarn2 = Yarn "Polyester" "Pitter Patter" 70
  let yarn3 = Yarn "Polyester" "Vanilla" 220
  print $ yarn1 == yarn2
  print $ yarn1 /= yarn3