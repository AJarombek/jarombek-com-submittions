{-
  Working with the IO type along with Functors and Applicatives
  Author: Andrew Jarombek
  Date: 5/27/2019
-}

-- Hide implementations of type classes and methods from the Prelude module
import Prelude hiding (Functor, Applicative, pure, (<*>), (<$>))

instance Functor IO where
  -- fmap :: (a -> b) -> IO a -> IO b
  fmap f mx = do { x <- mx; return (f x) }

instance Applicative IO where
  -- pure :: a -> IO a
  pure x = return x

main :: IO ()
main = do
  print $ show "hello"