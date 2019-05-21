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

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

-- Override the Either type from the Prelude module
data Either a b = Left a | Right b
  deriving (Eq, Ord, Show, Read)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

instance Functor (Either a) where
  -- fmap :: (a -> b) -> Either a -> Either b
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

main :: IO ()
main = do
  --
  print $ fmap (+1) [1,2,3] -- [2,3,4]
  print $ fmap (*2) [1,2,3] -- [2,4,6]
  print $ fmap show [1,2,3] -- ["1","2","3"]

  print $ fmap (+1) (Just 5) -- Just 6
  print $ fmap (+1) Nothing -- Nothing
  print $ fmap (*3) (Just 5) -- Just 15
  print $ fmap (*3) Nothing -- Nothing

  let num = Left 12 :: Either Int String
  print $ either (+31) length num -- 43

  -- print $ fmap (+2) (Right 2)
  -- print $ fmap (+2) (Left "hello")