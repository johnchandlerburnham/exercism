module Pangram (isPangram) where

import Data.Char (isAsciiLower, toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = 26 == (length $ nub $ filter isAsciiLower $ fmap toLower text)
