{-
  Working with Functors
  Author: Andrew Jarombek
  Date: 5/20/2019
-}

-- If I wanted to use my custom definition of Functor instead of the one from the Prelude,
-- simply hide the Functor Prelude import
import Prelude hiding (Functor, fmap, Either, Left, Right, either)

-- Override the Functor class defined in the Prelude module
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Make a list into a Functor
instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

-- Make the 'Maybe' type into a Functor
instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

-- Override the Either type from the Prelude module
data Either a b = Left a | Right b
  deriving (Eq, Ord, Show, Read)

{-|
  Apply one of two functions to a value depending on the type of the value.
-}
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

-- Make the 'Either' type into a Functor
instance Functor (Either a) where
  -- fmap :: (a -> b) -> Either a -> Either b
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

-- My own custom distance type
-- Functor types must have type constructors - https://stackoverflow.com/a/45235908.
data Distance a = Miles a | Kilometers a | Meters a
                deriving (Show, Read)

-- Since Functor types must have type constructors, the original Distance type can't be an instance of Functor.
data DistanceOrig = MilesOrig Float | KilometersOrig Float | MetersOrig Float

-- Make the 'Distance' type into a Functor
instance Functor Distance where
  -- fmap :: (a -> b) -> Distance a -> Distance b
  fmap f (Miles x) = Miles (f x)
  fmap f (Kilometers x) = Kilometers (f x)
  fmap f (Meters x) = Meters (f x)

{-|
  Increment a Functor type that holds an integer by one.
  Any type f that is an instance of class Functor can use the 'inc' function.
-}
inc :: Functor f => f Int -> f Int
inc x = fmap (+1) x

main :: IO ()
main = do
  -- Demonstrate basic map operations
  print $ map (+1) [1,2,3] -- [2,3,4]

  -- Testing list Functor instances
  print $ fmap (+1) [1,2,3] -- [2,3,4]
  print $ fmap (*2) [1,2,3] -- [2,4,6]
  print $ fmap show [1,2,3] -- ["1","2","3"]

  -- Testing Maybe Functor instances
  print $ fmap (+1) (Just 5) -- Just 6
  print $ fmap (+1) Nothing -- Nothing
  print $ fmap (*3) (Just 5) -- Just 15
  print $ fmap (*3) Nothing -- Nothing

  -- Learning the Either type
  -- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Either.html
  let num = Left 12 :: Either Int String
  let str = Right "hello" :: Either Int String

  -- Neither of these will compile
  -- let invalid_num = Left "andy" :: Either Int String
  -- let invalid_str = Right 12 :: Either Int String

  print $ either (+31) length num -- 43
  print $ either (+31) length str -- 5

  -- Testing Either Functor instances
  -- 'fmap' ignores left values, and applies a function to right values.  This occurs because the Functor instance for
  -- type Either is a partial application.
  print $ fmap length num -- Left 12
  print $ fmap length str -- Right 4

  let num2 = Right 2 :: Either String Int
  let str2 = Left "andy" :: Either String Int

  print $ fmap (*26) num2 -- Right 52
  print $ fmap (*26) str2 -- Left "andy"

  -- Testing Distance Functor instances
  print $ fmap (+1) (Miles 4.38) -- Miles 5.38
  print $ fmap (+1) (Kilometers 7.05) -- Kilometers 8.05
  print $ fmap (+1) (Meters 7005) -- Meters 7006
  print $ fmap (*3) (Miles 1) -- Miles 3

  -- Testing the 'inc' function which uses instances of class Functor
  print $ inc [1,2,3] -- [2,3,4]
  print $ inc (Just 5) -- Just 6
  print $ inc Nothing -- Nothing

  print $ inc num2 -- Right 3
  print $ inc str2 -- Left "andy"
  -- These are both invalid - the Right function needs to handle type Int, not String
  -- print $ inc num
  -- print $ inc str

  print $ inc (Miles 3) -- Miles 4