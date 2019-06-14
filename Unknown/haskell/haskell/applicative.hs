{-
  Working with Applicative Functors
  Author: Andrew Jarombek
  Date: 5/27/2019
-}

-- Hide implementations of type classes and methods from the Prelude module
import Prelude hiding (Applicative, pure, (<*>))

-- Type class for a functor with zero arguments.  Simply wraps a value in a type that is an instance of Functor
class Functor0 f where
  fmap0 :: a -> f a

-- Type class for a functor with a single argument.  This is equivalent to the normal fmap function and Functor type
class Functor1 f where
  fmap1 :: (a -> b) -> f a -> f b

-- Type class for a functor with two arguments.
class Functor2 f where
  fmap2 :: (a -> b -> c) -> f a -> f b -> f c

-- Type class for an Applicative with a pure and <*> function.  In order for a type to be an instance of Applicative,
-- it must also be an instance of Functor
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  -- (<$>) :: (Functor f) => (a -> b) -> f a -> f b

-- Make Maybe an instance of my Applicative type class (its already an instance of Functor from the Prelude module)
instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure x = Just x

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx

-- Make Either an instance of my Applicative type class (its already an instance of Functor from the Prelude module)
instance Applicative (Either a) where
  -- pure :: a -> Either a
  pure x = Right x

  -- (<*>) :: Either (a -> b) -> Either a -> Either b

  -- Definition which does not combine errors
  Left x <*> _ = Left x
  Right f <*> x = fmap f x

  -- Definition which combines errors - https://stackoverflow.com/a/23342577
  -- Right f <*> Right x = Right (f x)
  -- Left e <*> Right _ = Left e
  -- Right _ <*> Left e = Left e
  -- <> is a function of the Semigroup Type class which combines two values - https://wiki.haskell.org/Monoid
  -- Left e1 <*> Left e2 = Left (e1 <> e2)

-- instance Semigroup (Either a b) where
  -- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#Semigroup
  -- (<>) :: a -> a -> a
  -- Left _ <> b = b
  -- a <> _ = a

-- Maybe Maybe an instance of a functor with zero arguments.
instance Functor0 Maybe where
  -- fmap0 :: a -> f a
  -- fmap0 x = Just x
  fmap0 x = pure x

-- Maybe Maybe an instance of a functor with one argument.
instance Functor1 Maybe where
  -- fmap1 :: (a -> b) -> f a -> f b
  -- fmap1 f x = fmap f x
  fmap1 f x = pure f <*> x

-- Maybe Maybe an instance of a functor with two arguments.
instance Functor2 Maybe where
  -- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
  fmap2 f x y = pure f <*> x <*> y

main :: IO ()
main = do
  -- Testing the pure function defined in the Applicative type class
  print $ (pure :: a -> Maybe a) 1 -- Just 1
  print $ (pure :: a -> Maybe a) (Just 1 :: Maybe Int) -- Just (Just 1)

  let pure_maybe = pure :: a -> Maybe a
  print $ pure_maybe 1 -- Just 1
  print $ pure_maybe 12.31 -- Just 12.31
  print $ pure_maybe (Just 2) -- Just (Just 2)

  -- Testing pure combined with <*> for a Functor with a single argument
  print $ pure (+1) <*> Just 1 -- Just 2
  print $ pure (*3) <*> Just 2 -- Just 6
  print $ pure (+1) <*> Nothing -- Nothing

  -- Testing pure combined with <*> for a Functor with two arguments
  print $ pure (+) <*> Just 1 <*> Just 3 -- Just 4
  print $ pure (+) <*> Nothing <*> Just 3 -- Nothing
  print $ pure (+) <*> Just 3 <*> Nothing -- Nothing

  -- Testing the FunctorX type class instances
  print $ (fmap0 :: a -> Maybe a) 1 -- Just 1
  print $ fmap1 (+1) (Just 1) -- Just 2
  print $ fmap1 (+1) Nothing -- Nothing
  print $ fmap2 (+) (Just 2) (Just 2) -- Just 4
  print $ fmap2 (+) Nothing (Just 2) -- Nothing
  print $ fmap2 (+) (Just 2) Nothing -- Nothing

  -- Testing Applicative Either
  let pure_either = pure :: b -> Either String b
  print $ pure_either 5 -- Right 5
  print $ pure_either "Andy" -- Right "Andy"
  print $ pure_either [1,2,3] -- Right [1,2,3]
  print $ pure_either (++) <*> Right "Hello" <*> Right " World" -- "Hello World"