module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just $ go 0 n
 where
  go c n
    | n == 1    = c
    | even n    = go (c + 1) (n `div` 2)
    | otherwise = go (c + 1) (3 * n + 1)


