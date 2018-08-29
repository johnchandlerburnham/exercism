module DNA (nucleotideCounts) where

import Data.Map (Map, adjust, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = go xs $ fromList [('A', 0), ('C', 0), ('T', 0), ('G', 0)]
 where
  go [] m = Right m
  go (n:ns) m
   | n == 'A' = go ns $ adjust (+1) 'A' m
   | n == 'C' = go ns $ adjust (+1) 'C' m
   | n == 'T' = go ns $ adjust (+1) 'T' m
   | n == 'G' = go ns $ adjust (+1) 'G' m
   | otherwise = Left xs
