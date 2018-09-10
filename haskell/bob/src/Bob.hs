module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)

responseFor :: String -> String
responseFor x
  | allUpper a && lastIs '?' a = "Calm down, I know what I'm doing!"
  | lastIs '?' a               = "Sure."
  | allUpper a                 = "Whoa, chill out!"
  | a == ""                    = "Fine. Be that way!"
  | otherwise                  = "Whatever."
 where
  a = filter (not . isSpace) x
  allUpper xs = (\x -> all isUpper x && (not . null) x) $ filter isLetter xs
  lastIs _ [] = False
  lastIs a xs = last xs == a

