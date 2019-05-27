{-
  Working with Applicative Functors
  Author: Andrew Jarombek
  Date: 5/27/2019
-}

class Functor0 f where
  fmap0 :: a -> f a

class Functor1 f where
  fmap1 :: (a -> b) -> f a -> f b

class Functor2 f where
  fmap2 :: (a -> b -> c) -> f a -> f b -> f c

class Functor f => Applicative f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b

class Functor0Rewrite f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b
  fmap0 :: pure

class Functor1Rewrite f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b
  fmap0 g x :: pure g <*> x

class Functor2Rewrite f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b
  fmap0 g x y :: pure g <*> x <*> y

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx

main :: IO ()
main = do
  print $ show "hello"