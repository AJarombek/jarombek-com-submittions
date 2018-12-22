{-
  Explore Class inheritance in Haskell
  Author: Andrew Jarombek
  Date: 12/21/2018
-}

-- Types of fir trees
data FrasierFir = FrasierFir Int Int
data BalsamFir = BalsamFir Int Int
data DouglasFir = DouglasFir Int Int

-- Grades for different qualities of the fir trees
data Grade = Fair | Good | Excellent

{-|
 - A class for a biological tree.  Each tree must be able to calculate its height
 -}
class Tree a where
  -- Class operator
  height :: a -> Int

  -- Default definition
  height _ = 0

{-|
 - A class for a Christmas tree that inherits from the biological tree class.
 -}
class (Tree a) => ChristmasTree a where
  -- Class operator
  holiday_tree :: a -> Bool

  -- Default definition
  holiday_tree _ = True

{-|
 - A class for an evergreen tree that inherits from the biological tree class.
 -}
class (Tree a) => EvergreenTree a where
  -- Class operator
  leaf_persistence :: a -> Bool

  -- Default definition
  leaf_persistence _ = True

{-|
 - A class for a fir tree, which is both a Christmas tree and an evergreen tree.
 -}
class (ChristmasTree a, EvergreenTree a) => FirTree a where
  -- Class operators
  fragrance :: a -> Grade
  ease_to_decorate :: a -> Grade
  needle_retention :: a -> Grade

{-|
 - FrasierFir is an instance of FirTree (which inherits Tree, ChristmasTree, and EvergreenTree)
 -}
instance FirTree FrasierFir where
  fragrance a = Fair
  ease_to_decorate a = Good
  needle_retention a = Excellent

{-|
 - DouglasFir is an instance of FirTree (which inherits Tree, ChristmasTree, and EvergreenTree)
 -}
instance FirTree DouglasFir where
  fragrance a = Good
  ease_to_decorate a = Fair
  needle_retention a = Excellent

{-|
 - BalsamFir is an instance of FirTree (which inherits Tree, ChristmasTree, and EvergreenTree)
 -}
instance FirTree BalsamFir where
  fragrance a = Excellent
  ease_to_decorate a = Excellent
  needle_retention a = Fair

{- Main Function -}
main :: IO ()
main = do
  print $ ""