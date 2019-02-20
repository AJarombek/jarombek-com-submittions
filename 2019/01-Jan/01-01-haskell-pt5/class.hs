{-
  Explore Classes and Ad-hoc polymorphism in Haskell
  Author: Andrew Jarombek
  Date: 12/21/2018
-}

import Prelude hiding (Eq, Ord, (==))
import GHC.Classes (eqInt, neInt)

{-|
 - Create a class called Eq that checks for equality.  It has two operations, == and /=.
 - This class also exists in the standard Prelude module.
 -}
class Eq a where
  (==), (/=) :: a -> a -> Bool

  x /= y = not (x == y)

{-|
 - Create a class called Ord that inherits from the Eq class.  Besides for the inherited operations, it
 - has six operations.  Four are native operators and two are additional functions.
 -}
class (Eq a) => Ord a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a

  -- Default methods
  min x y | x <= y = x
          | otherwise = y

  max x y | x <= y = y
          | otherwise = x

{-|
 - Make Integer an instance of Eq.  Since I'm hiding Eq from the Prelude module, this instance must be
 - explicitly declared.
 - 'eqInt' and 'neInt' come from https://hackage.haskell.org/package/base-4.4.1.0/docs/src/GHC-Classes.html
 -}
instance Eq Int where
  (==) = eqInt
  (/=) = neInt

-- Create a type for fir trees of different species.  A tree needs to be provided a height in feet and inches
data FirTree = FrasierFir Int Int | BalsamFir Int Int | DouglasFir Int Int

{-|
 - Make FirTree an instance of Eq as well.  In order to be equal, the species of tree must be the same
 - and the trees must have the same height.
 -}
instance Eq FirTree where
  (FrasierFir f1 i1) == (FrasierFir f2 i2) = (f1 == f2) && (i1 == i2)
  (BalsamFir f1 i1) == (BalsamFir f2 i2) = (f1 == f2) && (i1 == i2)
  (DouglasFir f1 i1) == (DouglasFir f2 i2) = (f1 == f2) && (i1 == i2)
  _ == _ = False

{- Main Function -}
main :: IO ()
main = do
  print $ (1 :: Int) == (1 :: Int) -- True
  print $ (2 :: Int) == (1 :: Int) -- False

  let balsam1 = BalsamFir 6 2
  let balsam2 = BalsamFir 7 4
  let balsam3 = BalsamFir 6 2

  print $ balsam1 == balsam2 -- False
  print $ balsam1 == balsam3 -- True

  let frasier1 = FrasierFir 6 2
  let frasier2 = FrasierFir 7 4
  let frasier3 = FrasierFir 6 2

  print $ frasier1 == frasier2 -- False
  print $ frasier1 == frasier3 -- True
  print $ frasier1 == balsam1 -- False