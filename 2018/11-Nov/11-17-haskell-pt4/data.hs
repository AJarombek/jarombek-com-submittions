{-
  Work with new Haskell data types
  Author: Andrew Jarombek
  Date: 11/13/2018
-}

-- If I wanted to use my custom definition of Maybe instead of the one from the Prelude,
-- simply hide the Maybe & Just Prelude import
import Prelude hiding (Maybe, Just, Nothing)

-- Create a new 'Optional' data type for representing a value that 'Maybe' exists
-- Haskell provides an easy way for making types into instances of the Prelude classes Eq, Ord, Show, & Read
-- with the deriving keyword.  The following example makes 'Maybe' an instance of the Show & Read classes.
data Maybe a = Nothing | Just a
               deriving (Show, Read)

-- Create a new type representing different exercises.  'ExerciseType' can be converted to and from Strings
data ExerciseType = Run | Swim | Bike
                    deriving (Show, Read)

-- Create a new type representing different distances.  'Distance' can be converted to and from Strings
data Distance = Miles Float | Kilometers Float | Meters Float
                deriving (Show, Read)

{-|
  Create an exercise if a valid distance is provided.  If an invalid distance is provided, return Nothing.
-}
exercise :: ExerciseType -> Distance -> Maybe (ExerciseType, Distance)
exercise e (Miles x) | x > 0 = Just (e, Miles x)
                     | otherwise = Nothing
exercise e (Kilometers x) | x > 0 = Just (e, Kilometers x)
                          | otherwise = Nothing
exercise e (Meters x) | x > 0 = Just (e, Meters x)
                      | otherwise = Nothing

{- Main Function -}
main :: IO ()
main = do
  print $ exercise Run (Miles 1.05)
  print $ exercise Bike (Kilometers 3)
  print $ exercise Swim (Meters 120)

  print $ exercise Run (Miles 0)
  print $ exercise Run (Miles (-2.1))
  print $ exercise Bike (Kilometers (-2.1))
  print $ exercise Swim (Meters (-2.1))