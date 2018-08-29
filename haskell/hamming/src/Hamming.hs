module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
 | length xs == length ys = Just $ hammingDistance xs ys
 | otherwise = Nothing

hammingDistance :: Eq a => [a] -> [a] -> Int
hammingDistance xs ys = length $ filter ((==) False) $ zipWith (==) xs ys
