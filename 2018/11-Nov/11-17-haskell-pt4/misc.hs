{-
  Other miscellaneous code using types
  Author: Andrew Jarombek
  Date: 11/14/2018
-}

-- types can include type variables.  This is an example of a polymorphic type.
-- type 'Week' is a tuple that contains seven elements
type Week a = (a,a,a,a,a,a,a)

{-|
  Retrieve the item in the fourth element of the tuple (which represents Wednesday).
-}
wednesday :: Week a -> a
wednesday (_,_,_,w,_,_,_) = w

-- Data type constructors must have unique names across all types!
data Pets = Cat | Dog

{- Error: Multiple Declarations of 'Cat' -}
-- data Animal = Cat | Deer

{- Main Function -}
main :: IO ()
main = do
  -- milesWalked is a tuple of length seven, so its also a 'Week'
  let milesWalked = (1.2,4.0,6.7,2.0,0.0,0.0,7.0)
  print $ wednesday milesWalked -- 2.0

  let daysOfWeek = ("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  print $ wednesday daysOfWeek -- "Wednesday"

  let notAWeek = ("1","2","3",4,"5",6,"7")
  print $ notAWeek
  -- print $ wednesday notAWeek -- Fails because the tuple notAWeek does not match the 'Week' type