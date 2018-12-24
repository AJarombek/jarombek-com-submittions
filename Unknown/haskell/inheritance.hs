{-
  Explore Class inheritance in Haskell
  Author: Andrew Jarombek
  Date: 12/21/2018
-}

-- Types of fir trees
data FrasierFir = FrasierFir Int Int
                  deriving Show

data BalsamFir = BalsamFir Int Int
                 deriving Show

data DouglasFir = DouglasFir Int Int
                  deriving Show

-- Grades for different qualities of the fir trees
data Grade = Fair | Good | Excellent
             deriving Show

{-|
 - A class for a biological tree.  Each tree must be able to calculate its height
 -}
class Tree a where
  -- Class operator
  height :: a -> Int

  -- Default definition
  height _ = 0

{-|
 - A class for a Christmas tree which is an extension of the biological tree class.
 -}
class (Tree a) => ChristmasTree a where
  -- Class operator
  holiday_tree :: a -> Bool

  -- Default definition
  holiday_tree _ = True

{-|
 - A class for an evergreen tree which is an extension of the biological tree class.
 -}
class (Tree a) => EvergreenTree a where
  -- Class operator
  leaf_persistence :: a -> Bool

  -- Default definition
  leaf_persistence _ = True

{-|
 - A class for a fir tree, which is an extension of both a Christmas tree and an evergreen tree.
 -}
class (ChristmasTree a, EvergreenTree a) => FirTree a where
  -- Class operators
  fragrance :: a -> Grade
  ease_to_decorate :: a -> Grade
  needle_retention :: a -> Grade

{-|
 - FrasierFir is an instance of FirTree.  Since it is an instance of FirTree, it must also be an instance
 - of ChristmasTree, EvergreenTree, and Tree
 -}
instance FirTree FrasierFir where
  fragrance a = Fair
  ease_to_decorate a = Good
  needle_retention a = Excellent

instance ChristmasTree FrasierFir
instance EvergreenTree FrasierFir

instance Tree FrasierFir where
  height (FrasierFir x y) = (x * 12) + y

{-|
 - DouglasFir is an instance of FirTree.  Since it is an instance of FirTree, it must also be an instance
 - of ChristmasTree, EvergreenTree, and Tree
 -}
instance FirTree DouglasFir where
  fragrance a = Good
  ease_to_decorate a = Fair
  needle_retention a = Excellent

instance ChristmasTree DouglasFir
instance EvergreenTree DouglasFir

instance Tree DouglasFir where
  height (DouglasFir x y) = (x * 12) + y

{-|
 - BalsamFir is an instance of FirTree.  Since it is an instance of FirTree, it must also be an instance
 - of ChristmasTree, EvergreenTree, and Tree
 -}
instance FirTree BalsamFir where
  fragrance a = Excellent
  ease_to_decorate a = Excellent
  needle_retention a = Fair

instance ChristmasTree BalsamFir
instance EvergreenTree BalsamFir

instance Tree BalsamFir where
  height (BalsamFir x y) = (x * 12) + y

{- Main Function -}
main :: IO ()
main = do
  let frasier = FrasierFir 7 2

  print $ frasier -- FrasierFir 7 2
  print $ fragrance frasier -- Fair
  print $ ease_to_decorate frasier -- Good
  print $ needle_retention frasier -- Excellent
  print $ leaf_persistence frasier -- True
  print $ holiday_tree frasier -- True
  print $ height frasier -- 86

  let balsam = BalsamFir 5 6

  print $ balsam -- BalsamFir 5 6
  print $ fragrance balsam -- Excellent
  print $ ease_to_decorate balsam -- Excellent
  print $ needle_retention balsam -- Fair

  let douglas = DouglasFir 10 2

  print $ douglas -- DouglasFir 10 2
  print $ fragrance douglas -- Good
  print $ ease_to_decorate douglas -- Fair
  print $ needle_retention douglas -- Excellent