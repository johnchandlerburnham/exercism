module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (isLower, toLower)

isIsogram :: String -> Bool
isIsogram str = let s = filter isLower (toLower <$> str)
                in length (nub s) == length s
