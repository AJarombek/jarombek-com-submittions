{-
  Work with Haskell types
  Author: Andrew Jarombek
  Date: 11/13/2018
-}

-- Create a new Haskell type called Song which is a tuple
-- Haskell types declared with the 'type' keyword are aliases for existing types
type Song = (String, [String])

{-|
  Retrieve the title from a Song type.
-}
title :: Song -> String
title (t,as) = t

{-|
  Retrieve the list of artists from a Song type.
-}
artists :: Song -> [String]
artists (t,as) = as

{- Main Function -}
main :: IO ()
main = do
  let ech_ts = ("Enchanted", ["Taylor Swift"])
  let fri_mc = ("Anytime You Need a Friend", ["Mariah Carey"])
  let cry_fg = ("Big Girls Don't Cry", ["Fergie"])
  -- ..and https://bit.ly/2RTCZ17

  -- Print out the full songs
  print $ ech_ts
  print $ fri_mc
  print $ cry_fg

  -- Print out the song titles
  print $ title ech_ts
  print $ title fri_mc
  print $ title cry_fg

  -- Print out the songs artists
  print $ artists ech_ts
  print $ artists fri_mc
  print $ artists cry_fg